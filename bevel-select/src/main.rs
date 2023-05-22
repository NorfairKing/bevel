use crossterm::{
    event::{self, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use fuzzy_matcher::skim::SkimMatcherV2;
use fuzzy_matcher::FuzzyMatcher;
use ordered_float::OrderedFloat;
use std::{
    cmp::Reverse,
    collections::HashMap,
    env, io,
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};
use tui::{
    backend::{Backend, CrosstermBackend},
    layout::{Alignment, Constraint, Corner, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Span, Spans},
    widgets::{List, ListItem, ListState, Paragraph},
    Frame, Terminal,
};
use whoami::{hostname, username};

use sqlite::State;

trait QueryMaker {
    fn bind_count_query<'a>(&self, connection: &'a sqlite::Connection) -> sqlite::Statement<'a>;
    fn bind_load_query<'a>(
        &self,
        connection: &'a sqlite::Connection,
        limit: i64,
        offset: i64,
    ) -> sqlite::Statement<'a>;
}

struct CdQueryMaker {
    hostname: String,
    username: String,
}
impl CdQueryMaker {
    fn new() -> Self {
        let hostname: String = hostname();
        let username: String = username();

        CdQueryMaker { hostname, username }
    }
}
impl QueryMaker for CdQueryMaker {
    fn bind_count_query<'a>(&self, connection: &'a sqlite::Connection) -> sqlite::Statement<'a> {
        let mut statement = connection
            .prepare("SELECT COUNT(*) from command WHERE host = ? AND user = ?")
            .unwrap();
        statement.bind((1, self.hostname.as_str())).unwrap();
        statement.bind((2, self.username.as_str())).unwrap();
        statement
    }
    fn bind_load_query<'a>(
        &self,
        connection: &'a sqlite::Connection,
        limit: i64,
        offset: i64,
    ) -> sqlite::Statement<'a> {
        let mut statement = connection
            .prepare("SELECT workdir, begin FROM command WHERE host = ? AND user = ? ORDER BY begin DESC LIMIT ? OFFSET ?")
            .unwrap();
        statement.bind((1, self.hostname.as_str())).unwrap();
        statement.bind((2, self.username.as_str())).unwrap();
        statement.bind((3, limit)).unwrap();
        statement.bind((4, offset)).unwrap();
        statement
    }
}

struct RepeatQueryMaker {}
impl RepeatQueryMaker {
    fn new() -> Self {
        RepeatQueryMaker {}
    }
}
impl QueryMaker for RepeatQueryMaker {
    fn bind_count_query<'a>(&self, connection: &'a sqlite::Connection) -> sqlite::Statement<'a> {
        connection.prepare("SELECT COUNT(*) from command").unwrap()
    }
    fn bind_load_query<'a>(
        &self,
        connection: &'a sqlite::Connection,
        limit: i64,
        offset: i64,
    ) -> sqlite::Statement<'a> {
        let mut statement = connection
            .prepare("SELECT text, begin FROM command ORDER BY begin DESC LIMIT ? OFFSET ?")
            .unwrap();
        statement.bind((1, limit)).unwrap();
        statement.bind((2, offset)).unwrap();
        statement
    }
}
const CD_COMMAND: &'static str = "cd";
const REPEAT_COMMAND: &'static str = "repeat";
fn main() -> Result<(), io::Error> {
    let command = env::args().nth(1).expect("No command given.");
    let cd_maker;
    let repeat_maker;
    let query_maker: &dyn QueryMaker = match command.as_str() {
        CD_COMMAND => {
            cd_maker = CdQueryMaker::new();
            &cd_maker
        }
        REPEAT_COMMAND => {
            repeat_maker = RepeatQueryMaker::new();
            &repeat_maker
        }
        _ => {
            println!("Unknown command");
            return Ok(());
        }
    };

    // setup terminal
    enable_raw_mode()?;
    // Output the tui to stderr so we can capture stdout from the shell afterwards
    let mut stderr = io::stderr();
    execute!(stderr, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stderr);
    let mut terminal = Terminal::new(backend)?;

    let xdg_dirs = xdg::BaseDirectories::with_prefix("bevel").unwrap();
    let path = xdg_dirs.get_data_file("history.sqlite3");
    let open_flags = sqlite::OpenFlags::new().set_read_only();

    let connection = sqlite::Connection::open_with_flags(path, open_flags).unwrap();

    let app = App::new(&connection, query_maker);
    let res = run_app(&mut terminal, app);

    // restore terminal
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;

    let selection = res?;
    if let Some(selected) = selection {
        println!("{selected}");
    }
    Ok(())
}

fn run_app<B: Backend>(terminal: &mut Terminal<B>, mut app: App) -> io::Result<Option<String>> {
    // 1000 milliseconds is a second.
    let frames_per_second = 25;
    let tick_rate = Duration::from_millis(1000 / frames_per_second);

    let mut last_tick = Instant::now();
    loop {
        terminal.draw(|f| ui(f, &mut app))?;

        let more_rows_to_load = app.loaded < app.total;

        // We want to check for the next event.
        // If there are no more rows to load, then we can use the time that would normally spend in
        // timeout on loading more rows.
        //
        //
        // Wait for an event, or until the tick ends.
        let timeout = if more_rows_to_load {
            Duration::ZERO
        } else {
            tick_rate
                .checked_sub(last_tick.elapsed())
                .unwrap_or_else(|| Duration::from_secs(0))
        };

        // If there is an event, deal with it and finish the loop immediately.
        let event_available = crossterm::event::poll(timeout)?;
        if event_available {
            let event = event::read()?;
            if let Event::Key(key) = event {
                match key.code {
                    KeyCode::Up => app.select_next(),
                    KeyCode::Down => app.select_previous(),
                    KeyCode::Enter => return Ok(app.selected()),
                    KeyCode::Esc => return Ok(None),
                    KeyCode::Char(char) => app.append(char),
                    KeyCode::Backspace => app.remove(),
                    _ => {}
                }
            }
        }
        // If there was no event available, use the rest of the tick time to load more rows.
        else {
            // If there are more rows to load, we will try loading them
            // as long as we still have time within this tick, load some more rows
            while app.loaded < app.total && last_tick.elapsed() <= tick_rate {
                app.load_rows(1024);
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
        .constraints(
            [
                Constraint::Min(1),
                Constraint::Length(1),
                Constraint::Length(1),
            ]
            .as_ref(),
        )
        .vertical_margin(0)
        .horizontal_margin(1)
        .split(f.size());

    // The items at the top.
    let items: Vec<ListItem> = app
        .choices
        .top_items
        .iter()
        .enumerate()
        .map(|(ix, command)| {
            let style = if Some(ix) == app.list_state.selected() {
                Style::default()
                    .fg(Color::Rgb(0xa0, 0xa0, 0xa0))
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default().fg(Color::Yellow)
            };

            ListItem::new(command.key.clone()).style(style)
        })
        .collect();
    let choices_list = List::new(items)
        .start_corner(Corner::BottomLeft)
        .highlight_symbol("❯ ");
    f.render_stateful_widget(choices_list, chunks[0], &mut app.list_state);

    let search_text_span = Span::styled(
        &app.choices.search_text,
        Style::default().fg(Color::Rgb(0xa0, 0xa0, 0xa0)),
    );
    let width = search_text_span.width() as u16;
    let search_text_styled = vec![Spans::from(vec![search_text_span])];
    let search_text_widget = Paragraph::new(search_text_styled).alignment(Alignment::Left);
    let mut chunk_to_the_right = chunks[1];
    chunk_to_the_right.x += 2;
    chunk_to_the_right.width -= 2;
    f.render_widget(search_text_widget, chunk_to_the_right);
    f.set_cursor(chunk_to_the_right.x + width, chunk_to_the_right.y);

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
    f.render_widget(loaded_label, chunks[2]);
}

struct App<'a> {
    query_maker: &'a dyn QueryMaker,
    connection: &'a sqlite::Connection,
    list_state: ListState,
    choices: Choices,
    loaded: u64,
    total: u64,
}
impl<'a> App<'a> {
    pub fn new(connection: &'a sqlite::Connection, query_maker: &'a dyn QueryMaker) -> Self {
        let mut statement = query_maker.bind_count_query(connection);

        statement.next().unwrap();
        let total = statement.read::<i64, _>("COUNT(*)").unwrap();
        let mut list_state = ListState::default();
        list_state.select(Some(0));
        App {
            query_maker,
            connection,
            list_state,
            choices: Choices::new(String::new()),
            loaded: 0,
            total: total as u64,
        }
    }

    pub fn select_next(&mut self) {
        let i = match self.list_state.selected() {
            Some(i) => {
                if i >= self.choices.top_items.len() - 1 {
                    0
                } else {
                    i + 1
                }
            }
            None => 0,
        };
        self.list_state.select(Some(i));
    }

    pub fn select_previous(&mut self) {
        let i = match self.list_state.selected() {
            Some(i) => {
                if i == 0 {
                    self.choices.top_items.len() - 1
                } else {
                    i - 1
                }
            }
            None => 0,
        };
        self.list_state.select(Some(i));
    }

    pub fn selected(&self) -> Option<String> {
        self.list_state
            .selected()
            .and_then(|ix| self.choices.top_items.get(ix))
            .map(|c| c.key.clone())
    }

    pub fn append(&mut self, c: char) {
        self.choices.search_text.push(c);
        self.reset_search();
    }
    pub fn remove(&mut self) {
        self.choices.search_text.pop();
        self.reset_search();
    }

    fn reset_search(&mut self) {
        self.loaded = 0;
        let text = self.choices.search_text.clone();
        self.choices = Choices::new(text);
    }

    pub fn load_rows(&mut self, rows: u64) {
        let limit = rows as i64;
        let offset = self.loaded as i64;
        let mut statement = self
            .query_maker
            .bind_load_query(self.connection, limit, offset);

        while let Ok(State::Row) = statement.next() {
            self.loaded += 1;
            let workdir = statement.read::<String, _>(0).unwrap();
            let begin = statement.read::<i64, _>(1).unwrap();
            self.choices.add(workdir, begin);
        }
    }
}

struct Choices {
    now: i64,
    search_text: String,
    matcher: SkimMatcherV2,
    top_items: Vec<Choice>,
    item_scores: HashMap<String, f64>,
    minimum_score: f64,
}

const NANOSECONDS_IN_A_DAY: f64 = 86_400_000_000_000_f64;
const MAX_ITEMS: usize = 20;
impl Choices {
    pub fn new(search_text: String) -> Self {
        Choices {
            now: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos() as i64,
            search_text,
            matcher: SkimMatcherV2::default(),
            top_items: Vec::with_capacity(MAX_ITEMS),
            item_scores: HashMap::new(),
            minimum_score: 0.0,
        }
    }

    // Add a (workdir, begin) pair after computing its score
    pub fn add(&mut self, key: String, begin: i64) {
        let fuzziness = if self.search_text.is_empty() {
            1
        } else {
            self.matcher
                .fuzzy_match(&key, &self.search_text)
                .unwrap_or(0)
        };
        if fuzziness <= 0 {
            return;
        }

        // Compute the score of this item
        let timediff = (self.now - begin) as f64;
        let score = NANOSECONDS_IN_A_DAY / timediff;

        // Add to the item scores
        let total_score: f64 = *self
            .item_scores
            .entry(key.clone())
            .and_modify(|s| {
                *s += score;
            })
            .or_insert(score);

        if total_score > self.minimum_score {
            let choice = Choice {
                fuzziness,
                score: total_score,
                key: key.clone(),
            };
            // If the item is already there, remove it.
            for i in 0..self.top_items.len() {
                if key == self.top_items[i].key {
                    self.top_items.remove(i);
                    break;
                }
            }
            // Add the item again
            self.top_items.push(choice);
            // Sort by score
            self.top_items
                .sort_by_key(|c| (Reverse(c.fuzziness), Reverse(OrderedFloat(c.score))));
            // Remove any extra items
            if self.top_items.len() >= MAX_ITEMS {
                self.top_items.pop();
            }
            // There might be a new minimal top item, so we recompute the minimum score.
            self.recompute_minimum_score();
        }
    }
    fn recompute_minimum_score(&self) -> f64 {
        match self.top_items.last() {
            None => 0.0,
            // We can 'unwrap' because the top_items MUST be in the item_scores too.
            Some(least_top) => *self.item_scores.get(&least_top.key).unwrap(),
        }
    }
}
struct Choice {
    fuzziness: i64,
    score: f64,
    key: String,
}
