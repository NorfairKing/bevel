use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ordered_float::OrderedFloat;
use std::{
    cmp::Reverse,
    collections::HashMap,
    io,
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};
use tui::{
    backend::{Backend, CrosstermBackend},
    layout::{Alignment, Constraint, Corner, Direction, Layout},
    style::{Color, Style},
    text::{Span, Spans},
    widgets::{List, ListItem, ListState, Paragraph},
    Frame, Terminal,
};

use sqlite::State;

fn main() -> Result<(), io::Error> {
    // setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let open_flags = sqlite::OpenFlags::new().set_read_only();

    let connection = sqlite::Connection::open_with_flags(
        "/home/syd/.local/share/bevel/history.sqlite3",
        open_flags,
    )
    .unwrap();

    let app = App::new(&connection);
    let res = run_app(&mut terminal, app);

    // restore terminal
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    res
}

fn run_app<B: Backend>(terminal: &mut Terminal<B>, mut app: App) -> io::Result<()> {
    // 1000 milliseconds is a second.
    let frames_per_second = 30;
    let tick_rate = Duration::from_millis(1000 / frames_per_second);

    let mut last_tick = Instant::now();
    loop {
        terminal.draw(|f| ui(f, &mut app))?;

        // If there are still more rows to load.
        if app.loaded < app.total {
            // As long as we still have time within this tick, load some more rows
            while last_tick.elapsed() <= tick_rate {
                app.load_rows(256);
            }
        }

        // Wait for an event, or until the tick ends.
        let timeout = tick_rate
            .checked_sub(last_tick.elapsed())
            .unwrap_or_else(|| Duration::from_secs(0));
        if crossterm::event::poll(timeout)? {
            if let Event::Key(key) = event::read()? {
                match key.code {
                    KeyCode::Char('q') => return Ok(()),
                    _ => {}
                }
            }
        }

        if last_tick.elapsed() >= tick_rate {
            last_tick = Instant::now();
        }
    }
}

fn ui<B: Backend>(f: &mut Frame<B>, app: &mut App) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(90), Constraint::Length(1)].as_ref())
        .split(f.size());

    let items: Vec<ListItem> = app
        .choices
        .top_items
        .iter()
        .map(|command| ListItem::new(command.clone()).style(Style::default().fg(Color::Yellow)))
        .collect();
    let choices_list = List::new(items)
        .start_corner(Corner::BottomLeft)
        .highlight_symbol("> ");
    f.render_stateful_widget(choices_list, chunks[0], &mut app.list_state);

    // The counter on the bottom right
    let loaded_colour = if app.loaded >= app.total {
        Color::Green
    } else {
        Color::Red
    };
    let loaded_text = vec![Spans::from(vec![
        Span::styled(
            format!("{}", app.loaded),
            Style::default().fg(loaded_colour),
        ),
        Span::styled(" / ", Style::default().fg(Color::Yellow)),
        Span::styled(format!("{}", app.total), Style::default().fg(Color::Green)),
    ])];
    let loaded_label = Paragraph::new(loaded_text).alignment(Alignment::Right);
    f.render_widget(loaded_label, chunks[1]);
}

struct App<'a> {
    connection: &'a sqlite::Connection,
    list_state: ListState,
    choices: Choices,
    loaded: u64,
    total: u64,
}
impl<'a> App<'a> {
    pub fn new(connection: &'a sqlite::Connection) -> Self {
        const STARTING_COUNT_QUERY: &str = "SELECT COUNT(*) from command";
        let mut statement = connection.prepare(STARTING_COUNT_QUERY).unwrap();
        statement.next().unwrap();
        let total = statement.read::<i64, _>("COUNT(*)").unwrap();
        let mut list_state = ListState::default();
        list_state.select(Some(0));
        App {
            connection: &connection,
            list_state: list_state,
            choices: Choices::new(),
            loaded: 0,
            total: total as u64,
        }
    }

    pub fn load_rows(&mut self, rows: u64) {
        let offset = self.loaded as i64;
        let limit = rows as i64;
        const ROW_LOADING_QUERY: &str = "SELECT workdir, begin FROM command LIMIT ? OFFSET ?";
        let mut statement = self.connection.prepare(ROW_LOADING_QUERY).unwrap();
        statement.bind((1, limit)).unwrap();
        statement.bind((2, offset)).unwrap();

        while let Ok(State::Row) = statement.next() {
            self.loaded += 1;
            let workdir = statement.read::<String, _>("workdir").unwrap();
            let begin = statement.read::<i64, _>("begin").unwrap();
            self.choices.add(workdir, begin);
        }
    }
}

struct Choices {
    now: i64,
    top_items: Vec<String>,
    item_scores: HashMap<String, f64>,
}

const NANOSECONDS_IN_A_DAY: f64 = 86400_000_000_000_f64;
const MAX_ITEMS: usize = 20;

impl Choices {
    pub fn new() -> Self {
        Choices {
            now: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos() as i64,
            top_items: Vec::with_capacity(MAX_ITEMS),
            item_scores: HashMap::new(),
        }
    }

    // Add a (workdir, begin) pair after computing its score
    pub fn add(&mut self, key: String, begin: i64) {
        // Compute the score of this item
        let timediff = (self.now - begin) as f64;
        let score = NANOSECONDS_IN_A_DAY / timediff;

        // Add to the item scores
        let total_score: f64 = self
            .item_scores
            .entry(key.clone())
            .and_modify(|s| {
                *s += score;
            })
            .or_insert(score)
            .clone();

        // // Minimum score to end up in the top_items.
        let minimum_score = if self.top_items.len() < MAX_ITEMS {
            0_f64
        } else {
            let least_top = self.top_items.last().unwrap();
            // We can 'unwrap' because the top_items MUST be in the item_scores too.
            *self.item_scores.get(least_top).unwrap()
        };

        if total_score > minimum_score {
            self.top_items.push(key);
            self.top_items.dedup();
            self.top_items
                .sort_by_key(|k| Reverse(OrderedFloat(*self.item_scores.get(k).unwrap())));
            while self.top_items.len() >= MAX_ITEMS {
                self.top_items.pop();
            }
        }
    }
}
