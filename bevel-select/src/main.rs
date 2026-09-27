use crossterm::{
    cursor::Show,
    event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::{Backend, CrosstermBackend},
    layout::{Alignment, Constraint, Direction, Layout, Position},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{List, ListDirection, ListItem, ListState, Paragraph},
    Frame, Terminal,
};
use std::{
    collections::HashSet,
    env, io,
    process::ExitCode,
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};
use whoami::{hostname, username};

use sqlite::State;

mod choices;

use choices::{clamped_selection, next_selection, previous_selection, Choices, Subject};

/// Where the picker was opened: the directory the ranking prefers, and the
/// machine and account that `cd` filters on.
struct Context {
    /// A shell outlives the directory it sits in, which is removed from under
    /// it often enough, and the history is still worth offering when it has
    /// been. `repeat` then simply has no directory to prefer.
    workdir: Option<String>,
    hostname: String,
    username: String,
}

impl Context {
    fn new() -> Self {
        let workdir: Option<String> = env::current_dir()
            .ok()
            .and_then(|d| d.into_os_string().into_string().ok());
        let hostname: String = hostname().expect("Unable to read hostname");
        let username: String = username().expect("Unable to read username");

        Context {
            workdir,
            hostname,
            username,
        }
    }
}
enum SomeQueryMaker {
    Cd(Context),
    Repeat(Context),
    RepeatLocal(Context),
}
impl SomeQueryMaker {
    fn subject(&self) -> Subject {
        match self {
            SomeQueryMaker::Cd(_) => Subject::Directory,
            SomeQueryMaker::Repeat(_) => Subject::Command,
            SomeQueryMaker::RepeatLocal(_) => Subject::Command,
        }
    }
    fn bind_count_query<'a>(&self, connection: &'a sqlite::Connection) -> sqlite::Statement<'a> {
        match self {
            SomeQueryMaker::Cd(context) => {
                let mut statement = connection
                    .prepare("SELECT COUNT(*) from command WHERE host = ? AND user = ?")
                    .unwrap();
                statement.bind((1, context.hostname.as_str())).unwrap();
                statement.bind((2, context.username.as_str())).unwrap();
                statement
            }
            SomeQueryMaker::Repeat(_) => {
                connection.prepare("SELECT COUNT(*) from command").unwrap()
            }
            SomeQueryMaker::RepeatLocal(context) => {
                let mut statement = connection
                    .prepare("SELECT COUNT(*) from command WHERE workdir = ?")
                    .unwrap();
                if let Some(workdir) = &context.workdir {
                    statement.bind((1, workdir.as_str())).unwrap();
                }
                statement
            }
        }
    }
    /// Prepare a page of the history, most recent command first.
    ///
    /// The boundary is inclusive, so a page repeats the commands that share the
    /// timestamp it started from. An exclusive boundary would page straight
    /// past all but one of them, and ordering on the id as well to avoid that
    /// costs a sort that the index cannot serve. The caller drops the repeats.
    fn bind_load_query<'a>(
        &self,
        connection: &'a sqlite::Connection,
        last_begin_loaded: Option<i64>,
    ) -> sqlite::Statement<'a> {
        // The parameters are numbered alike across the modes even though no one
        // query uses them all, so that the weight expression can be shared:
        // ?1 the page boundary, ?2 the directory, ?3 the machine, ?4 the
        // account. Each mode binds only the ones its own query mentions.
        let boundary = if last_begin_loaded.is_some() {
            "begin <= ?1 AND"
        } else {
            ""
        };
        let query = match self {
            // Ranking directories, so no directory counts for more. Host and
            // user are filtered here rather than preferred, because a path from
            // another machine may not be a path on this one.
            SomeQueryMaker::Cd(_) => format!(
                "SELECT workdir, begin, {weight}, id FROM command \
                 WHERE {boundary} host = ?3 AND user = ?4 \
                 ORDER BY begin DESC LIMIT 8096",
                weight = choices::occurrence_weight_sql(None)
            ),
            // The only mode with no condition of its own to hang the boundary
            // on, and the only one where the directory is a preference. With no
            // directory to be in, there is none to prefer, and the rest of the
            // ranking stands.
            SomeQueryMaker::Repeat(context) => {
                let weight = choices::occurrence_weight_sql(
                    context.workdir.as_ref().map(|_| "workdir = ?2"),
                );
                match last_begin_loaded {
                    Some(_) => format!(
                        "SELECT text, begin, {weight}, id FROM command \
                         WHERE begin <= ?1 ORDER BY begin DESC LIMIT 8096"
                    ),
                    None => format!(
                        "SELECT text, begin, {weight}, id FROM command \
                         ORDER BY begin DESC LIMIT 8096"
                    ),
                }
            }
            // Every row is from this directory already, so preferring it would
            // scale the whole list and change nothing.
            SomeQueryMaker::RepeatLocal(_) => format!(
                "SELECT text, begin, {weight}, id FROM command \
                 WHERE {boundary} workdir = ?2 \
                 ORDER BY begin DESC LIMIT 8096",
                weight = choices::occurrence_weight_sql(None)
            ),
        };
        let context = self.context();
        let mut statement = connection.prepare(&query).unwrap();
        if let Some(begin) = last_begin_loaded {
            statement.bind((1, begin)).unwrap();
        }
        // Cd is the only one that looks at the machine and the account, and it
        // filters on them rather than preferring them.
        match self {
            SomeQueryMaker::Cd(_) => {
                statement.bind((3, context.hostname.as_str())).unwrap();
                statement.bind((4, context.username.as_str())).unwrap();
            }
            SomeQueryMaker::Repeat(_) | SomeQueryMaker::RepeatLocal(_) => {
                if let Some(workdir) = &context.workdir {
                    statement.bind((2, workdir.as_str())).unwrap();
                }
            }
        }
        statement
    }
    fn context(&self) -> &Context {
        match self {
            SomeQueryMaker::Cd(context) => context,
            SomeQueryMaker::Repeat(context) => context,
            SomeQueryMaker::RepeatLocal(context) => context,
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
        "cd" => SomeQueryMaker::Cd(Context::new()),
        "repeat" => SomeQueryMaker::Repeat(Context::new()),
        "repeat-local" => {
            let context = Context::new();
            // This mode is nothing but the directory, so without one there is
            // nothing to show. The others carry on.
            if context.workdir.is_none() {
                eprintln!("No working directory to look in. Has it been removed?");
                return ExitCode::FAILURE;
            }
            SomeQueryMaker::RepeatLocal(context)
        }
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

    let restored = restore_terminal();
    // Whatever went wrong in the picker says more than a failure to tidy up
    // after it, and neither is worth throwing away a selection over.
    let selection = selection?;
    if let Err(error) = restored {
        eprintln!("Could not restore the terminal: {error}");
    }
    Ok(selection)
}

fn restore_terminal() -> io::Result<()> {
    disable_raw_mode()?;
    execute!(io::stderr(), LeaveAlternateScreen, Show)
}

// Ratatui lets a backend choose its own error type.  The one here writes to a
// terminal, so it fails the way writing to anything else does.
fn run_app<B: Backend<Error = io::Error>>(
    terminal: &mut Terminal<B>,
    mut app: App,
) -> io::Result<Option<String>> {
    // 1000 milliseconds is a second.
    let frames_per_second = 25;
    let tick_rate = Duration::from_millis(1000 / frames_per_second);

    let mut last_tick = Instant::now();
    loop {
        terminal.draw(|f| ui(f, &mut app))?;

        let more_rows_to_load = !app.finished_loading;

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
            while !app.finished_loading && last_tick.elapsed() <= tick_rate {
                app.load_rows();
            }
        }

        if last_tick.elapsed() >= tick_rate {
            last_tick = Instant::now();
        }
    }
}

/// How many lines of one command the list will show before saying how many it
/// left out.
const MAX_LINES: usize = 5;

/// The lines of a command to put in the list, with a note standing in for the
/// rest of them.
///
/// A command gets a row per line, so a long one would fill the list on its own
/// and crowd out everything it was being compared against.  The command itself
/// is untouched: all of it goes onto the command line when it is chosen.
fn shown_lines(command: &str) -> Vec<String> {
    let total = command.lines().count();
    let mut lines: Vec<String> = command
        .lines()
        .take(MAX_LINES)
        .map(str::to_string)
        .collect();
    let left_out = total.saturating_sub(MAX_LINES);
    if left_out > 0 {
        let lines_or_line = if left_out == 1 { "line" } else { "lines" };
        lines.push(format!("\u{2026} {left_out} more {lines_or_line}"));
    }
    // A list item of no lines has no height, and would be invisible rather
    // than empty.
    if lines.is_empty() {
        lines.push(String::new());
    }
    lines
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
    let chord = key
        .modifiers
        .intersects(KeyModifiers::CONTROL | KeyModifiers::ALT);
    match key.code {
        // Raw mode stops the terminal turning Ctrl-C into a signal, so the
        // picker has to quit on it itself.
        KeyCode::Char('c' | 'd') if key.modifiers.contains(KeyModifiers::CONTROL) => Action::Cancel,
        // A terminal that sends 0x08 for backspace is indistinguishable from
        // one sending Ctrl-H, and readline deletes on both.
        KeyCode::Char('h') if key.modifiers.contains(KeyModifiers::CONTROL) => Action::Remove,
        // Typing the plain letter of a chord would be worse than doing nothing.
        KeyCode::Char(_) if chord => Action::Ignore,
        KeyCode::Char(char) => Action::Append(char),
        KeyCode::Up => Action::SelectNext,
        KeyCode::Down => Action::SelectPrevious,
        KeyCode::Enter => Action::Accept,
        KeyCode::Esc => Action::Cancel,
        KeyCode::Backspace => Action::Remove,
        _ => Action::Ignore,
    }
}

fn ui(f: &mut Frame, app: &mut App) {
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
        .split(f.area());

    // The items at the top.
    let items: Vec<ListItem> = app
        .choices
        .top_items()
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

            let lines: Vec<Line> = shown_lines(&command.key)
                .into_iter()
                .map(Line::from)
                .collect();
            ListItem::new(lines).style(style)
        })
        .collect();
    let choices_list = List::new(items)
        .direction(ListDirection::BottomToTop)
        .highlight_symbol("❯ ");
    f.render_stateful_widget(choices_list, chunks[0], &mut app.list_state);

    let search_text_span = Span::styled(
        app.choices.search_text(),
        Style::default().fg(Color::Rgb(0xa0, 0xa0, 0xa0)),
    );
    let width = search_text_span.width() as u16;
    let search_text_styled = vec![Line::from(vec![search_text_span])];
    let search_text_widget = Paragraph::new(search_text_styled).alignment(Alignment::Left);
    let mut chunk_to_the_right = chunks[1];
    chunk_to_the_right.x += 2;
    chunk_to_the_right.width -= 2;
    f.render_widget(search_text_widget, chunk_to_the_right);
    f.set_cursor_position(Position::new(
        chunk_to_the_right.x + width,
        chunk_to_the_right.y,
    ));

    // The counter on the bottom right
    let loaded_colour = if app.finished_loading {
        Color::Green
    } else {
        Color::Red
    };
    let loaded_text = vec![Line::from(vec![
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
    finished_loading: bool,
    last_begin_loaded: Option<i64>,
    /// The commands already loaded that started at `last_begin_loaded`, so that
    /// the overlap between one page and the next can be dropped.
    ids_loaded_at_last_begin: HashSet<i64>,
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
            choices: Choices::new(now_nanos(), query_maker.subject()),
            last_begin_loaded: None,
            ids_loaded_at_last_begin: HashSet::new(),
            finished_loading: false,
            loaded: 0,
            total: total as u64,
        }
    }

    pub fn select_next(&mut self) {
        let selection = next_selection(self.list_state.selected(), self.choices.top_items().len());
        self.list_state.select(selection);
    }

    pub fn select_previous(&mut self) {
        let selection =
            previous_selection(self.list_state.selected(), self.choices.top_items().len());
        self.list_state.select(selection);
    }

    pub fn selected(&self) -> Option<String> {
        self.list_state
            .selected()
            .and_then(|ix| self.choices.top_items().get(ix))
            .map(|c| c.key.clone())
    }

    pub fn append(&mut self, c: char) {
        self.choices.append(c);
        self.select_first();
    }

    pub fn remove(&mut self) {
        if self.choices.remove() {
            self.select_first();
        }
    }

    /// The list is a different list after the search text changes, so the item
    /// that was highlighted has nothing to do with the one now in its place.
    fn select_first(&mut self) {
        let selection = clamped_selection(Some(0), self.choices.top_items().len());
        self.list_state.select(selection);
    }

    fn clamp_selection(&mut self) {
        let selection =
            clamped_selection(self.list_state.selected(), self.choices.top_items().len());
        self.list_state.select(selection);
    }

    pub fn load_rows(&mut self) {
        let mut statement = self
            .query_maker
            .bind_load_query(self.connection, self.last_begin_loaded);

        // Stepping is unwrapped like every other call against the database
        // here.  Treating an error as the end of the history instead would
        // report a truncated history as the whole of it.
        let mut new_in_page = 0;
        while statement.next().unwrap() == State::Row {
            let key = statement.read::<String, _>(0).unwrap();
            let begin = statement.read::<i64, _>(1).unwrap();
            let weight = statement.read::<f64, _>(2).unwrap();
            let id = statement.read::<i64, _>(3).unwrap();

            if self.last_begin_loaded == Some(begin) {
                if !self.ids_loaded_at_last_begin.insert(id) {
                    continue;
                }
            } else {
                self.last_begin_loaded = Some(begin);
                self.ids_loaded_at_last_begin.clear();
                self.ids_loaded_at_last_begin.insert(id);
            }

            self.choices.add(key, begin, weight);

            self.loaded += 1;
            new_in_page += 1;
        }

        // The total is counted separately from the pages, so it cannot say when
        // the history has run out.  A page with nothing new in it can mean
        // that, or that more commands share one timestamp than fit in a page,
        // which leaves nowhere to page on to.  Either way there is no next
        // page to ask for.
        if new_in_page == 0 {
            self.finished_loading = true;
        }

        self.choices.recompute_top_items();
        self.clamp_selection();
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
    fn shows_a_short_command_whole() {
        assert_eq!(shown_lines("nix flake check"), vec!["nix flake check"]);
        assert_eq!(
            shown_lines("sudo swapoff -a\nsudo swapon -a"),
            vec!["sudo swapoff -a", "sudo swapon -a"]
        );
    }

    #[test]
    fn shows_a_command_of_exactly_the_cap_without_a_note() {
        let command = (1..=MAX_LINES)
            .map(|i| format!("line {i}"))
            .collect::<Vec<String>>()
            .join("\n");
        assert_eq!(shown_lines(&command).len(), MAX_LINES);
        assert_eq!(shown_lines(&command).last().unwrap(), "line 5");
        assert!(!shown_lines(&command).iter().any(|l| l.contains("more")));
    }

    #[test]
    fn says_how_many_lines_of_a_long_command_it_left_out() {
        let command = (1..=8)
            .map(|i| format!("line {i}"))
            .collect::<Vec<String>>()
            .join("\n");
        assert_eq!(
            shown_lines(&command),
            vec![
                "line 1",
                "line 2",
                "line 3",
                "line 4",
                "line 5",
                "\u{2026} 3 more lines"
            ]
        );
    }

    #[test]
    fn counts_a_single_left_out_line_as_one_line() {
        let command = (1..=MAX_LINES + 1)
            .map(|i| format!("line {i}"))
            .collect::<Vec<String>>()
            .join("\n");
        assert_eq!(
            shown_lines(&command).last().unwrap(),
            "\u{2026} 1 more line"
        );
    }

    #[test]
    fn gives_an_empty_command_a_row_to_be_empty_in() {
        assert_eq!(shown_lines(""), vec![""]);
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
    fn deletes_on_the_other_byte_a_terminal_may_send_for_backspace() {
        assert_eq!(
            action_for(press(KeyCode::Backspace, KeyModifiers::NONE)),
            Action::Remove
        );
        assert_eq!(
            action_for(press(KeyCode::Char('h'), KeyModifiers::CONTROL)),
            Action::Remove
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

    /// A history in which every timestamp is shared by `per_timestamp`
    /// commands, so that a page boundary is bound to fall inside a group.
    fn history_of_simultaneous_commands(count: usize, per_timestamp: usize) -> sqlite::Connection {
        let connection = sqlite::Connection::open(":memory:").unwrap();
        connection
            .execute(
                "CREATE TABLE command (id INTEGER PRIMARY KEY, text VARCHAR NOT NULL, begin INTEGER NOT NULL, end INTEGER, workdir VARCHAR NOT NULL, user VARCHAR NOT NULL, host VARCHAR NOT NULL, exit INTEGER, server_id INTEGER)",
            )
            .unwrap();
        connection.execute("BEGIN").unwrap();
        for i in 0..count {
            let mut statement = connection
                .prepare("INSERT INTO command (text, begin, workdir, user, host, exit) VALUES (?, ?, '/tmp', 'user', 'host', 0)")
                .unwrap();
            statement
                .bind((1, format!("command {i}").as_str()))
                .unwrap();
            statement.bind((2, (i / per_timestamp) as i64 + 1)).unwrap();
            statement.next().unwrap();
        }
        connection.execute("COMMIT").unwrap();
        connection
    }

    fn load_everything(connection: &sqlite::Connection) -> App<'_> {
        // Leaked so that it outlives the app that borrows it, which a test may
        // do and a run may not.
        let query_maker: &'static SomeQueryMaker =
            Box::leak(Box::new(SomeQueryMaker::Repeat(Context {
                workdir: Some(String::from("/tmp")),
                hostname: String::from("host"),
                username: String::from("user"),
            })));
        let mut app = App::new(connection, query_maker);
        while !app.finished_loading {
            app.load_rows();
        }
        app
    }

    /// What an occurrence is worth is worked out by the database, so running it
    /// is the only way to cover the rule.
    #[test]
    fn works_out_in_the_database_what_an_occurrence_is_worth() {
        let connection = sqlite::Connection::open(":memory:").unwrap();
        connection
            .execute("CREATE TABLE command (id INTEGER PRIMARY KEY, exit INTEGER)")
            .unwrap();
        connection
            .execute("INSERT INTO command (id, exit) VALUES (1, 0), (2, 1), (3, NULL)")
            .unwrap();

        let mut weights: Vec<f64> = Vec::new();
        for here in [None, Some("1")] {
            let query = format!(
                "SELECT {} FROM command ORDER BY id",
                choices::occurrence_weight_sql(here)
            );
            let mut statement = connection.prepare(&query).unwrap();
            while statement.next().unwrap() == State::Row {
                weights.push(statement.read::<f64, _>(0).unwrap());
            }
        }

        // The second pass counts every row as being in this directory.
        let here = choices::here_factor();
        assert_eq!(weights, vec![2.0, 1.0, 0.5, 2.0 * here, here, 0.5 * here]);
    }

    /// Whether a command was run in the directory the picker was opened in is
    /// also worked out by the database, and only the whole load shows it.
    #[test]
    fn weighs_only_the_commands_run_here_more_heavily() {
        let connection = sqlite::Connection::open(":memory:").unwrap();
        connection
            .execute(
                "CREATE TABLE command (id INTEGER PRIMARY KEY, text VARCHAR NOT NULL, begin INTEGER NOT NULL, end INTEGER, workdir VARCHAR NOT NULL, user VARCHAR NOT NULL, host VARCHAR NOT NULL, exit INTEGER, server_id INTEGER)",
            )
            .unwrap();
        connection
            .execute(
                "INSERT INTO command (text, begin, workdir, user, host, exit) VALUES \
                 ('here', 1, '/here', 'user', 'host', 0), \
                 ('there', 1, '/there', 'user', 'host', 0)",
            )
            .unwrap();
        let query_maker: &'static SomeQueryMaker =
            Box::leak(Box::new(SomeQueryMaker::Repeat(Context {
                workdir: Some(String::from("/here")),
                hostname: String::from("host"),
                username: String::from("user"),
            })));

        let mut app = App::new(&connection, query_maker);
        while !app.finished_loading {
            app.load_rows();
        }

        // Both ran at the same moment, so the whole difference is the
        // directory. What that is worth exactly is pinned where the database
        // works it out; this is that it reaches the ranking at all.
        let offered: Vec<&str> = app
            .choices
            .top_items()
            .iter()
            .map(|c| c.key.as_str())
            .collect();
        assert_eq!(offered, vec!["here", "there"]);
    }

    /// Cd offers the directories of this account on this machine, and only
    /// those. Its query is the one that binds the machine and the account, and
    /// nothing else exercises it.
    #[test]
    fn cd_offers_the_directories_of_this_account_on_this_machine() {
        let connection = sqlite::Connection::open(":memory:").unwrap();
        connection
            .execute(
                "CREATE TABLE command (id INTEGER PRIMARY KEY, text VARCHAR NOT NULL, begin INTEGER NOT NULL, end INTEGER, workdir VARCHAR NOT NULL, user VARCHAR NOT NULL, host VARCHAR NOT NULL, exit INTEGER, server_id INTEGER)",
            )
            .unwrap();
        connection
            .execute(
                "INSERT INTO command (text, begin, workdir, user, host, exit) VALUES \
                 ('a', 1, '/mine', 'me', 'here', 0), \
                 ('b', 2, '/another-machine', 'me', 'elsewhere', 0), \
                 ('c', 3, '/another-account', 'you', 'here', 0)",
            )
            .unwrap();
        let query_maker: &'static SomeQueryMaker =
            Box::leak(Box::new(SomeQueryMaker::Cd(Context {
                workdir: Some(String::from("/mine")),
                hostname: String::from("here"),
                username: String::from("me"),
            })));

        let mut app = App::new(&connection, query_maker);
        while !app.finished_loading {
            app.load_rows();
        }

        let offered: Vec<&str> = app
            .choices
            .top_items()
            .iter()
            .map(|c| c.key.as_str())
            .collect();
        assert_eq!(offered, vec!["/mine"]);
        assert_eq!(app.total, 1);
    }

    /// Repeat-local offers the commands run in this directory, and only those,
    /// whichever machine or account they came from.
    #[test]
    fn repeat_local_offers_the_commands_run_in_this_directory() {
        let connection = sqlite::Connection::open(":memory:").unwrap();
        connection
            .execute(
                "CREATE TABLE command (id INTEGER PRIMARY KEY, text VARCHAR NOT NULL, begin INTEGER NOT NULL, end INTEGER, workdir VARCHAR NOT NULL, user VARCHAR NOT NULL, host VARCHAR NOT NULL, exit INTEGER, server_id INTEGER)",
            )
            .unwrap();
        connection
            .execute(
                "INSERT INTO command (text, begin, workdir, user, host, exit) VALUES \
                 ('here', 1, '/here', 'me', 'here', 0), \
                 ('synced', 2, '/here', 'you', 'elsewhere', 0), \
                 ('elsewhere', 3, '/there', 'me', 'here', 0)",
            )
            .unwrap();
        let query_maker: &'static SomeQueryMaker =
            Box::leak(Box::new(SomeQueryMaker::RepeatLocal(Context {
                workdir: Some(String::from("/here")),
                hostname: String::from("here"),
                username: String::from("me"),
            })));

        let mut app = App::new(&connection, query_maker);
        while !app.finished_loading {
            app.load_rows();
        }

        let mut offered: Vec<&str> = app
            .choices
            .top_items()
            .iter()
            .map(|c| c.key.as_str())
            .collect();
        offered.sort_unstable();
        assert_eq!(offered, vec!["here", "synced"]);
        assert_eq!(app.total, 2);
    }

    /// A shell outlives the directory it sits in. Repeating a command must
    /// still work from one that has been removed, with no directory preferred
    /// and the rest of the ranking standing.
    #[test]
    fn repeats_from_a_directory_that_is_no_longer_there() {
        let connection = sqlite::Connection::open(":memory:").unwrap();
        connection
            .execute(
                "CREATE TABLE command (id INTEGER PRIMARY KEY, text VARCHAR NOT NULL, begin INTEGER NOT NULL, end INTEGER, workdir VARCHAR NOT NULL, user VARCHAR NOT NULL, host VARCHAR NOT NULL, exit INTEGER, server_id INTEGER)",
            )
            .unwrap();
        connection
            .execute(
                "INSERT INTO command (text, begin, workdir, user, host, exit) VALUES \
                 ('once', 1, '/gone', 'me', 'here', 0), \
                 ('twice', 2, '/elsewhere', 'me', 'here', 0), \
                 ('twice', 3, '/elsewhere', 'me', 'here', 0)",
            )
            .unwrap();
        let query_maker: &'static SomeQueryMaker =
            Box::leak(Box::new(SomeQueryMaker::Repeat(Context {
                workdir: None,
                hostname: String::from("here"),
                username: String::from("me"),
            })));

        let mut app = App::new(&connection, query_maker);
        while !app.finished_loading {
            app.load_rows();
        }

        let offered: Vec<&str> = app
            .choices
            .top_items()
            .iter()
            .map(|c| c.key.as_str())
            .collect();
        assert_eq!(offered, vec!["twice", "once"]);
    }

    /// The page boundary is bound as a different parameter from the rest, and
    /// only when there is one, so every mode has to page as well as start.
    #[test]
    fn every_mode_pages_through_a_history_that_does_not_fit_in_one_page() {
        for query_maker in [
            SomeQueryMaker::Cd(Context {
                workdir: Some(String::from("/tmp")),
                hostname: String::from("host"),
                username: String::from("user"),
            }),
            SomeQueryMaker::Repeat(Context {
                workdir: Some(String::from("/tmp")),
                hostname: String::from("host"),
                username: String::from("user"),
            }),
            SomeQueryMaker::RepeatLocal(Context {
                workdir: Some(String::from("/tmp")),
                hostname: String::from("host"),
                username: String::from("user"),
            }),
        ] {
            let count = 9000;
            let connection = history_of_simultaneous_commands(count, 1);

            let mut app = App::new(&connection, &query_maker);
            while !app.finished_loading {
                app.load_rows();
            }

            assert_eq!(app.total, count as u64);
            assert_eq!(app.loaded, count as u64);
        }
    }

    #[test]
    fn loads_every_command_when_commands_share_a_timestamp() {
        let count = 9000;
        let connection = history_of_simultaneous_commands(count, 3);

        let app = load_everything(&connection);

        assert_eq!(app.total, count as u64);
        assert_eq!(app.loaded, count as u64);
    }

    #[test]
    fn loads_every_command_when_no_two_share_a_timestamp() {
        let count = 9000;
        let connection = history_of_simultaneous_commands(count, 1);

        let app = load_everything(&connection);

        assert_eq!(app.total, count as u64);
        assert_eq!(app.loaded, count as u64);
    }

    /// More commands sharing one timestamp than fit in a page leaves nowhere to
    /// page on to. Loading stops rather than asking for that page forever.
    #[test]
    fn stops_loading_when_more_commands_share_a_timestamp_than_fit_in_a_page() {
        let connection = history_of_simultaneous_commands(9000, 9000);

        let app = load_everything(&connection);

        assert!(app.loaded < app.total);
    }
}
