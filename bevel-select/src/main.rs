use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use std::{
    io,
    time::{Duration, Instant},
};
use tui::{
    backend::{Backend, CrosstermBackend},
    layout::{Alignment, Constraint, Direction, Layout},
    style::{Color, Style},
    text::{Span, Spans},
    widgets::Paragraph,
    Frame, Terminal,
};

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

    let app = App::new(connection);
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
    let frames_per_second = 4;
    let tick_rate = Duration::from_millis(1000 / frames_per_second);
    let mut last_tick = Instant::now();
    loop {
        terminal.draw(|f| ui(f, &mut app))?;

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
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(100)].as_ref())
        .split(f.size());
    let loaded_text = vec![Spans::from(vec![
        Span::styled(format!("{}", app.loaded), Style::default().fg(Color::Red)),
        Span::styled(" / ", Style::default().fg(Color::Yellow)),
        Span::styled(format!("{}", app.total), Style::default().fg(Color::Green)),
    ])];
    let loaded_label = Paragraph::new(loaded_text).alignment(Alignment::Right);
    f.render_widget(loaded_label, chunks[0]);
}

struct App {
    loaded: u64,
    total: u64,
}
const STARTING_COUNT_QUERY: &str = "SELECT COUNT(*) from command";
impl App {
    pub fn new(connection: sqlite::Connection) -> Self {
        let mut statement = connection.prepare(STARTING_COUNT_QUERY).unwrap();
        statement.next().unwrap();
        let total = statement.read::<i64, _>("COUNT(*)").unwrap();
        App {
            loaded: 0,
            total: total as u64,
        }
    }
}
