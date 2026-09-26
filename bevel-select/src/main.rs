use crossterm::{
    cursor::Show,
    event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use std::{
    env, io,
    process::ExitCode,
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

mod choices;

use choices::{next_selection, previous_selection, Choices};

struct CdQueryMaker {
    hostname: String,
    username: String,
}

impl CdQueryMaker {
    fn new() -> Self {
        let hostname: String = hostname().expect("Unable to read hostname");
        let username: String = username().expect("Unable to read username");

        CdQueryMaker { hostname, username }
    }
}

struct RepeatLocalQueryMaker {
    workdir: String,
}

impl RepeatLocalQueryMaker {
    fn new() -> Self {
        let workdir: String = env::current_dir()
            .unwrap()
            .into_os_string()
            .into_string()
            .unwrap();

        RepeatLocalQueryMaker { workdir }
    }
}
enum SomeQueryMaker {
    Cd(CdQueryMaker),
    Repeat,
    RepeatLocal(RepeatLocalQueryMaker),
}
impl SomeQueryMaker {
    fn bind_count_query<'a>(&self, connection: &'a sqlite::Connection) -> sqlite::Statement<'a> {
        match self {
            SomeQueryMaker::Cd(cqm) => {
                let mut statement = connection
                    .prepare("SELECT COUNT(*) from command WHERE host = ? AND user = ?")
                    .unwrap();
                statement.bind((1, cqm.hostname.as_str())).unwrap();
                statement.bind((2, cqm.username.as_str())).unwrap();
                statement
            }
            SomeQueryMaker::Repeat => connection.prepare("SELECT COUNT(*) from command").unwrap(),
            SomeQueryMaker::RepeatLocal(rlqm) => {
                let mut statement = connection
                    .prepare("SELECT COUNT(*) from command WHERE workdir = ?")
                    .unwrap();
                statement.bind((1, rlqm.workdir.as_str())).unwrap();
                statement
            }
        }
    }
    fn bind_load_query<'a>(
        &self,
        connection: &'a sqlite::Connection,
        last_begin: Option<i64>,
    ) -> sqlite::Statement<'a> {
        match self {
            SomeQueryMaker::Cd(cqm) => {
                if let Some(last) = last_begin {
                    let mut statement = connection
                        .prepare("SELECT workdir, begin, exit FROM command WHERE begin < ? AND host = ? AND user = ? ORDER BY begin DESC LIMIT 8096")
                        .unwrap();
                    statement.bind((1, last)).unwrap();
                    statement.bind((2, cqm.hostname.as_str())).unwrap();
                    statement.bind((3, cqm.username.as_str())).unwrap();
                    statement
                } else {
                    let mut statement = connection
                        .prepare("SELECT workdir, begin, exit FROM command WHERE host = ? AND user = ? ORDER BY begin DESC LIMIT 8096")
                        .unwrap();
                    statement.bind((1, cqm.hostname.as_str())).unwrap();
                    statement.bind((2, cqm.username.as_str())).unwrap();
                    statement
                }
            }
            SomeQueryMaker::Repeat => {
                if let Some(last) = last_begin {
                    let mut statement = connection
                        .prepare("SELECT text, begin, exit FROM command WHERE begin < ? ORDER BY begin DESC LIMIT 8096")
                        .unwrap();
                    statement.bind((1, last)).unwrap();
                    statement
                } else {
                    connection
                        .prepare(
                            "SELECT text, begin, exit FROM command ORDER BY begin DESC LIMIT 8096",
                        )
                        .unwrap()
                }
            }
            SomeQueryMaker::RepeatLocal(rlqm) => {
                if let Some(last) = last_begin {
                    let mut statement = connection
                        .prepare("SELECT text, begin, exit FROM command WHERE begin < ? AND workdir = ? ORDER BY begin DESC LIMIT 8096")
                        .unwrap();
                    statement.bind((1, last)).unwrap();
                    statement.bind((2, rlqm.workdir.as_str())).unwrap();
                    statement
                } else {
                    let mut statement = connection
                        .prepare("SELECT text, begin, exit FROM command WHERE workdir = ? ORDER BY begin DESC LIMIT 8096")
                        .unwrap();
                    statement.bind((1, rlqm.workdir.as_str())).unwrap();
                    statement
                }
            }
        }
    }
}
const USAGE: &str = "Usage: bevel-select (cd | repeat | repeat-local)";

/// Anything but the selection goes to stderr, because the shell bindings
/// substitute stdout straight into a `cd` or into the command line.
fn main() -> ExitCode {
    let Some(command) = env::args().nth(1) else {
        eprintln!("No command given.");
        eprintln!("{USAGE}");
        return ExitCode::FAILURE;
    };
    let query_maker: SomeQueryMaker = match command.as_str() {
        "cd" => SomeQueryMaker::Cd(CdQueryMaker::new()),
        "repeat" => SomeQueryMaker::Repeat,
        "repeat-local" => SomeQueryMaker::RepeatLocal(RepeatLocalQueryMaker::new()),
        _ => {
            eprintln!("Unknown command: {command}");
            eprintln!("{USAGE}");
            return ExitCode::FAILURE;
        }
    };

    // Open the history before touching the terminal, so that a missing one is
    // a message the user can read instead of a panic painted into a screen
    // that is torn down immediately afterwards.
    let xdg_dirs = xdg::BaseDirectories::with_prefix("bevel");
    let Some(path) = xdg_dirs.find_data_file("history.sqlite3") else {
        eprintln!("No bevel history found. Is bevel set up in this shell?");
        return ExitCode::FAILURE;
    };
    let open_flags = sqlite::OpenFlags::new().with_read_only();
    let connection = match sqlite::Connection::open_with_flags(&path, open_flags) {
        Ok(connection) => connection,
        Err(error) => {
            eprintln!("Could not read {}: {error}", path.display());
            return ExitCode::FAILURE;
        }
    };

    match select(&connection, &query_maker) {
        Ok(None) => ExitCode::SUCCESS,
        Ok(Some(selected)) => {
            println!("{selected}");
            ExitCode::SUCCESS
        }
        Err(error) => {
            eprintln!("{error}");
            ExitCode::FAILURE
        }
    }
}

/// Show the picker, leaving the terminal the way it was found.
fn select(
    connection: &sqlite::Connection,
    query_maker: &SomeQueryMaker,
) -> io::Result<Option<String>> {
    enable_raw_mode()?;
    // A panic unwinds past the restore below, which would leave the terminal in
    // raw mode inside the alternate screen, where the panic message cannot even
    // be read.
    let panicked_before = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let _ = restore_terminal();
        panicked_before(info);
    }));

    // Draw to stderr so that the shell can capture the selection from stdout.
    let mut stderr = io::stderr();
    execute!(stderr, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stderr);
    let mut terminal = Terminal::new(backend)?;

    let app = App::new(connection, query_maker);
    let selection = run_app(&mut terminal, app);

    restore_terminal()?;
    selection
}

fn restore_terminal() -> io::Result<()> {
    disable_raw_mode()?;
    execute!(io::stderr(), LeaveAlternateScreen, Show)
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
                match action_for(key) {
                    Action::SelectNext => app.select_next(),
                    Action::SelectPrevious => app.select_previous(),
                    Action::Accept => return Ok(app.selected()),
                    Action::Cancel => return Ok(None),
                    Action::Append(char) => app.append(char),
                    Action::Remove => app.remove(),
                    Action::Ignore => {}
                }
            }
        }
        // If there was no event available, use the rest of the tick time to load more rows.
        else {
            // If there are more rows to load, we will try loading them
            // as long as we still have time within this tick, load some more rows
            while app.loaded < app.total && last_tick.elapsed() <= tick_rate {
                app.load_rows();
            }
        }

        if last_tick.elapsed() >= tick_rate {
            last_tick = Instant::now();
        }
    }
}

/// What a key press means, kept apart from carrying it out so that it can be
/// tested without a terminal.
#[derive(Debug, PartialEq, Eq)]
enum Action {
    SelectNext,
    SelectPrevious,
    Accept,
    Cancel,
    Append(char),
    Remove,
    Ignore,
}

fn action_for(key: KeyEvent) -> Action {
    if key.kind == KeyEventKind::Release {
        return Action::Ignore;
    }
    // Raw mode stops the terminal turning Ctrl-C into a signal, so the picker
    // has to quit on it itself.  Typing the plain letter of a chord would be
    // worse than doing nothing, so the rest are ignored rather than typed.
    if key.modifiers.contains(KeyModifiers::CONTROL) {
        return match key.code {
            KeyCode::Char('c') | KeyCode::Char('d') => Action::Cancel,
            _ => Action::Ignore,
        };
    }
    if key.modifiers.contains(KeyModifiers::ALT) {
        return Action::Ignore;
    }
    match key.code {
        KeyCode::Up => Action::SelectNext,
        KeyCode::Down => Action::SelectPrevious,
        KeyCode::Enter => Action::Accept,
        KeyCode::Esc => Action::Cancel,
        KeyCode::Char(char) => Action::Append(char),
        KeyCode::Backspace => Action::Remove,
        _ => Action::Ignore,
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
    query_maker: &'a SomeQueryMaker,
    connection: &'a sqlite::Connection,
    list_state: ListState,
    choices: Choices,
    last_begin_loaded: Option<i64>,
    loaded: u64,
    total: u64,
}
impl<'a> App<'a> {
    pub fn new(connection: &'a sqlite::Connection, query_maker: &'a SomeQueryMaker) -> Self {
        let mut statement = query_maker.bind_count_query(connection);

        statement.next().unwrap();
        let total = statement.read::<i64, _>("COUNT(*)").unwrap();
        let mut list_state = ListState::default();
        list_state.select(Some(0));
        App {
            query_maker,
            connection,
            list_state,
            choices: Choices::new(String::new(), now_nanos()),
            last_begin_loaded: None,
            loaded: 0,
            total: total as u64,
        }
    }

    pub fn select_next(&mut self) {
        let selection = next_selection(self.list_state.selected(), self.choices.top_items.len());
        self.list_state.select(selection);
    }

    pub fn select_previous(&mut self) {
        let selection =
            previous_selection(self.list_state.selected(), self.choices.top_items.len());
        self.list_state.select(selection);
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
        self.last_begin_loaded = None;
        let text = self.choices.search_text.clone();
        self.choices = Choices::new(text, now_nanos());
    }

    pub fn load_rows(&mut self) {
        let mut statement = self
            .query_maker
            .bind_load_query(self.connection, self.last_begin_loaded);

        while let Ok(State::Row) = statement.next() {
            let workdir = statement.read::<String, _>(0).unwrap();
            let begin = statement.read::<i64, _>(1).unwrap();
            let exit = statement.read::<Option<i64>, _>(2).unwrap();

            self.choices.add(workdir, begin, exit);

            self.loaded += 1;
            self.last_begin_loaded = Some(begin);
        }
    }
}

fn now_nanos() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos() as i64
}

#[cfg(test)]
mod tests {
    use super::*;

    fn press(code: KeyCode, modifiers: KeyModifiers) -> KeyEvent {
        KeyEvent::new(code, modifiers)
    }

    #[test]
    fn quits_on_the_chords_that_quit_everything_else() {
        assert_eq!(
            action_for(press(KeyCode::Char('c'), KeyModifiers::CONTROL)),
            Action::Cancel
        );
        assert_eq!(
            action_for(press(KeyCode::Char('d'), KeyModifiers::CONTROL)),
            Action::Cancel
        );
        assert_eq!(
            action_for(press(KeyCode::Esc, KeyModifiers::NONE)),
            Action::Cancel
        );
    }

    #[test]
    fn does_not_type_the_letter_of_a_chord() {
        assert_eq!(
            action_for(press(KeyCode::Char('a'), KeyModifiers::CONTROL)),
            Action::Ignore
        );
        assert_eq!(
            action_for(press(KeyCode::Char('x'), KeyModifiers::ALT)),
            Action::Ignore
        );
    }

    #[test]
    fn types_letters_including_capitals() {
        assert_eq!(
            action_for(press(KeyCode::Char('c'), KeyModifiers::NONE)),
            Action::Append('c')
        );
        assert_eq!(
            action_for(press(KeyCode::Char('C'), KeyModifiers::SHIFT)),
            Action::Append('C')
        );
    }

    #[test]
    fn moves_accepts_and_deletes() {
        assert_eq!(
            action_for(press(KeyCode::Up, KeyModifiers::NONE)),
            Action::SelectNext
        );
        assert_eq!(
            action_for(press(KeyCode::Down, KeyModifiers::NONE)),
            Action::SelectPrevious
        );
        assert_eq!(
            action_for(press(KeyCode::Enter, KeyModifiers::NONE)),
            Action::Accept
        );
        assert_eq!(
            action_for(press(KeyCode::Backspace, KeyModifiers::NONE)),
            Action::Remove
        );
    }

    #[test]
    fn ignores_a_key_being_let_go_of() {
        let mut key = press(KeyCode::Char('c'), KeyModifiers::NONE);
        key.kind = KeyEventKind::Release;
        assert_eq!(action_for(key), Action::Ignore);
    }
}
