//! What the ranking has to satisfy.
//!
//! Commands are ordered by how well they match the search text, and among
//! those that match it equally well, by how much they are used:
//!
//! ```text
//! order by  (fuzziness, use)  largest first, ties settled on the command
//!
//! use     = sum over the occurrences of one command of
//!               exit_weight * 2^(-age_in_days / HALF_LIFE_IN_DAYS)
//!           age_in_days = max(0, now - begin), in days
//!           exit_weight = 2 succeeded, 1 failed, 0.5 running or interrupted
//! ```
//!
//! Each requirement below says what breaks when it does not hold.
//!
//! # Use
//!
//! * It must not depend on the search text. This is what lets the history be
//!   read once per run instead of once per keystroke: a new search text
//!   re-matches what has already been read rather than reading it again.
//!
//! * It must not depend on the order the occurrences arrive in, nor on where
//!   the batches between them are cut. A batch ends when a frame runs out of
//!   time, so anything that depended on the batching would depend on how
//!   loaded the machine was. A sum of independent terms gives this: exactly
//!   for the batching, which does not reorder anything, and up to
//!   floating-point associativity for the order.
//!
//! * No occurrence may contribute an infinity, a NaN, or a negative amount,
//!   whatever its timestamp, including a timestamp at or after the moment the
//!   picker opened. A history gathered on a machine whose clock has moved
//!   backwards must still rank. Bounding each occurrence by its exit weight
//!   gives this, and is why no command has to be dropped for being
//!   unrankable.
//!
//! * An occurrence must never lower the use of its command, and occurrences
//!   at one moment must count for as much as their number. Otherwise running
//!   a command would be a reason to stop offering it.
//!
//! * Of two occurrences alike but for their age, the more recent must count
//!   for at least as much.
//!
//! * Multiplying every use by one common factor must leave the ranking alone,
//!   which is what moving the moment the picker opened does. Otherwise the
//!   list would be ordered differently depending on when it was opened, and a
//!   command would drift through it while the history stood still. Only uses
//!   are ever compared against each other, so this holds for any decay that
//!   depends on age alone; dividing by the age does not, because it is not
//!   such a decay.
//!
//! # Matching
//!
//! * Fuzziness must be a function of the search text and the command alone,
//!   and give the same answer however much the matcher has been used before.
//!
//! * A command that does not match the search text at all must never be
//!   offered, however much it is used.
//!
//! # Putting the two together
//!
//! * Matching decides, and use only settles the ties. This is the whole of
//!   the trade, and it is deliberate: it is what keeps a command last run
//!   three years ago reachable, because once enough has been typed that it
//!   matches better than anything else, nothing recent can bury it.
//!
//! * What it costs is the other direction. Any difference in fuzziness,
//!   however small and however spurious, outranks any amount of use. That is
//!   only safe while the matcher is not asked to produce differences it
//!   cannot justify: preferring a match near the front of a path invented a
//!   seven point difference out of nothing and put every directory under
//!   /nix/store above every project directory, which is why `Subject` exists.
//!   Any further matching option has to be weighed the same way.
//!
//! * The order must be total, and must not depend on the order the
//!   occurrences arrived in, so the command text settles whatever ties are
//!   left.
//!
//! * The list must depend on the search text alone and not on the way it was
//!   arrived at: typing a character and deleting it again must leave no
//!   trace.
//!
//! * Typing a character must only ever take matches away. This is what lets a
//!   keystroke narrow the candidates that survived the last one instead of
//!   looking at every command again.

use nucleo_matcher::pattern::{Atom, AtomKind, CaseMatching, Normalization};
use nucleo_matcher::{Config, Matcher, Utf32Str};
use ordered_float::OrderedFloat;
use std::{cmp::Ordering, collections::HashMap};

const NANOSECONDS_IN_A_DAY: f64 = 86_400_000_000_000_f64;
const MAX_ITEMS: usize = 20;

/// An occurrence is worth half as much for every this many days of its age.
const HALF_LIFE_IN_DAYS: f64 = 30.0;

#[derive(Debug, Clone, PartialEq)]
/// A command to offer, and the two numbers it was ordered by.
///
/// Only the command itself is anyone else's business; the other two are here
/// so that the tests can assert the order rather than infer it.
pub struct Choice {
    fuzziness: u32,
    usage: f64,
    pub key: String,
}

/// One distinct command, with the use of all its occurrences added up.
///
/// The use does not depend on the search text, which is what lets the
/// database be read only once per run.
struct Entry {
    key: String,
    usage: f64,
}

/// An entry that matches the search text, with the fuzziness of that match.
#[derive(Clone, Copy)]
struct Candidate {
    entry: usize,
    fuzziness: u32,
}

/// What the choices are, which decides whether matching near the start of one
/// counts for more than matching further in.
pub enum Subject {
    /// A command is usually recognised by how it starts.
    Command,
    /// A directory is not. The front of a path is boilerplate shared by
    /// thousands of them, so preferring a match there ranks every directory
    /// under /nix/store above every project directory for the search text
    /// "nix", however long ago the store paths were visited.
    Directory,
}

pub struct Choices {
    now: i64,
    search_text: String,
    matcher: Matcher,
    /// The search text prepared for matching, or `None` while it is empty.
    needle: Option<Atom>,
    /// Scratch space the matcher needs to look at a command.
    haystack: Vec<char>,
    entries: Vec<Entry>,
    entry_indices: HashMap<String, usize>,
    candidates: Vec<Candidate>,
    top_items: Vec<Choice>,
}

impl Choices {
    pub fn new(now: i64, subject: Subject) -> Self {
        let mut config = Config::DEFAULT;
        config.prefer_prefix = match subject {
            Subject::Command => true,
            Subject::Directory => false,
        };
        Choices {
            now,
            search_text: String::new(),
            matcher: Matcher::new(config),
            needle: None,
            haystack: Vec::new(),
            entries: Vec::new(),
            entry_indices: HashMap::new(),
            candidates: Vec::new(),
            top_items: Vec::with_capacity(MAX_ITEMS),
        }
    }

    /// Add one command occurrence.
    ///
    /// Occurrences are added whether or not they match the search text, so that
    /// editing the search text never requires reading them again.
    pub fn add(&mut self, key: String, begin: i64, exit: Option<i64>) {
        let key = without_trailing_whitespace(key);
        let usage = self.usage(begin, exit);
        match self.entry_indices.get(&key) {
            Some(&index) => self.entries[index].usage += usage,
            None => {
                let index = self.entries.len();
                let fuzziness = fuzziness(
                    &mut self.matcher,
                    &mut self.haystack,
                    self.needle.as_ref(),
                    &key,
                );
                self.entry_indices.insert(key.clone(), index);
                self.entries.push(Entry { key, usage });
                if let Some(fuzziness) = fuzziness {
                    self.candidates.push(Candidate {
                        entry: index,
                        fuzziness,
                    });
                }
            }
        }
    }

    pub fn append(&mut self, c: char) {
        self.search_text.push(c);
        self.rebuild_needle();
        // A command that does not contain the search text as a subsequence
        // cannot contain a longer one either, so adding a character can only
        // take matches away and the commands that already fail to match need
        // not be looked at again.
        let mut candidates = std::mem::take(&mut self.candidates);
        let Choices {
            matcher,
            haystack,
            needle,
            entries,
            ..
        } = self;
        candidates.retain_mut(|candidate| {
            match fuzziness(
                matcher,
                haystack,
                needle.as_ref(),
                &entries[candidate.entry].key,
            ) {
                Some(fuzziness) => {
                    candidate.fuzziness = fuzziness;
                    true
                }
                None => false,
            }
        });
        self.candidates = candidates;
        self.recompute_top_items();
    }

    /// Returns whether there was a character to remove.
    pub fn remove(&mut self) -> bool {
        if self.search_text.pop().is_some() {
            self.rebuild_needle();
            // Removing a character can bring matches back, so every command has
            // to be considered again.
            let Choices {
                matcher,
                haystack,
                needle,
                entries,
                ..
            } = self;
            let candidates: Vec<Candidate> = entries
                .iter()
                .enumerate()
                .filter_map(|(index, entry)| {
                    fuzziness(matcher, haystack, needle.as_ref(), &entry.key).map(|fuzziness| {
                        Candidate {
                            entry: index,
                            fuzziness,
                        }
                    })
                })
                .collect();
            self.candidates = candidates;
            self.recompute_top_items();
            true
        } else {
            false
        }
    }

    /// Bring the top items back in line with the use and the search text.
    ///
    /// Call this after adding occurrences. Adding does not do it itself, so
    /// that a whole batch of occurrences costs only one pass over the
    /// candidates.
    ///
    /// The candidates are read in the order they were discovered and never
    /// reordered, so that this walks the entries from front to back rather
    /// than jumping around them.
    pub fn recompute_top_items(&mut self) {
        let entries = &self.entries;
        let mut top: Vec<Candidate> = Vec::with_capacity(MAX_ITEMS);

        for candidate in &self.candidates {
            let full = top.len() >= MAX_ITEMS;
            if full && compare(entries, candidate, &top[MAX_ITEMS - 1]) != Ordering::Less {
                continue;
            }
            let position =
                top.partition_point(|other| compare(entries, other, candidate) == Ordering::Less);
            if full {
                top.pop();
            }
            top.insert(position, *candidate);
        }

        self.top_items = top
            .iter()
            .map(|candidate| {
                let entry = &entries[candidate.entry];
                Choice {
                    fuzziness: candidate.fuzziness,
                    usage: entry.usage,
                    key: entry.key.clone(),
                }
            })
            .collect();
    }

    /// `exit_weight * 2^(-age_in_days / HALF_LIFE_IN_DAYS)`: what one
    /// occurrence of a command is worth.
    ///
    /// A timestamp at or after the moment the picker opened counts as one from
    /// that moment, rather than being worth more than any amount of use or
    /// having to be thrown away, so that a history gathered while the clock
    /// moved backwards still ranks.
    fn usage(&self, begin: i64, exit: Option<i64>) -> f64 {
        let exit_weight = match exit {
            // If the command is still running or was interrupted, it's less relevant.
            None => 0.5f64,
            // If the command is succesful, it's more relevant.
            Some(0) => 2f64,
            Some(_) => 1f64,
        };
        let age_in_days = (self.now - begin).max(0) as f64 / NANOSECONDS_IN_A_DAY;
        exit_weight * (-age_in_days / HALF_LIFE_IN_DAYS).exp2()
    }

    pub fn search_text(&self) -> &str {
        &self.search_text
    }

    /// The best commands for the search text, best first.
    pub fn top_items(&self) -> &[Choice] {
        &self.top_items
    }

    /// Prepare the search text for matching, which only has to be done when it
    /// changes rather than once per command.
    fn rebuild_needle(&mut self) {
        self.needle = if self.search_text.is_empty() {
            None
        } else {
            Some(Atom::new(
                &self.search_text,
                CaseMatching::Smart,
                Normalization::Smart,
                AtomKind::Fuzzy,
                false,
            ))
        };
    }

    #[cfg(test)]
    fn matching_keys(&self) -> Vec<&str> {
        let mut keys: Vec<&str> = self
            .candidates
            .iter()
            .map(|candidate| self.entries[candidate.entry].key.as_str())
            .collect();
        keys.sort_unstable();
        keys
    }
}

/// A command without the trailing whitespace a tab completion leaves behind.
///
/// Otherwise a command typed out and the same command completed with a tab are
/// two different commands, each getting its own share of the use that belongs
/// to one of them.
fn without_trailing_whitespace(key: String) -> String {
    let trimmed = key.trim_end();
    // Most commands have nothing to trim, and those need not be copied.
    if trimmed.len() == key.len() {
        key
    } else {
        trimmed.to_string()
    }
}

/// How well a command matches the search text, or `None` when it does not
/// match at all.
///
/// Both answers depend only on the command and the search text.  This takes
/// the matcher apart from the rest of `Choices` so that a pass can hold the
/// entries and the matcher at the same time.
fn fuzziness(
    matcher: &mut Matcher,
    haystack: &mut Vec<char>,
    needle: Option<&Atom>,
    key: &str,
) -> Option<u32> {
    match needle {
        // An empty search text matches every command, all equally well.
        None => Some(0),
        Some(needle) => needle
            .score(Utf32Str::new(key, haystack), matcher)
            .map(u32::from),
    }
}

/// Order the candidates best-first: best match, then most used, then by
/// command so that the order never depends on how the occurrences arrived.
fn compare(entries: &[Entry], a: &Candidate, b: &Candidate) -> Ordering {
    let left = &entries[a.entry];
    let right = &entries[b.entry];
    b.fuzziness
        .cmp(&a.fuzziness)
        .then_with(|| OrderedFloat(right.usage).cmp(&OrderedFloat(left.usage)))
        .then_with(|| left.key.cmp(&right.key))
}

/// The index to select when moving towards the top of the screen.
///
/// The list is rendered from the bottom up, so this counts up.
pub fn next_selection(selected: Option<usize>, len: usize) -> Option<usize> {
    if len == 0 {
        return None;
    }
    match selected {
        Some(i) if i + 1 < len => Some(i + 1),
        _ => Some(0),
    }
}

/// The index to select when moving towards the bottom of the screen.
pub fn previous_selection(selected: Option<usize>, len: usize) -> Option<usize> {
    if len == 0 {
        return None;
    }
    match selected {
        Some(0) | None => Some(len - 1),
        Some(i) => Some(i - 1),
    }
}

/// Keep the selection on an item: inside the list once it has shrunk under
/// the selection, and back on the first item once there is anything to select.
pub fn clamped_selection(selected: Option<usize>, len: usize) -> Option<usize> {
    if len == 0 {
        return None;
    }
    match selected {
        None => Some(0),
        Some(i) => Some(i.min(len - 1)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    const DAY: i64 = 86_400_000_000_000;
    const HALF_LIFE: i64 = (HALF_LIFE_IN_DAYS as i64) * DAY;
    const NOW: i64 = 4 * HALF_LIFE;

    type Row = (String, i64, Option<i64>);

    fn choices_for(search_text: &str, rows: &[Row]) -> Choices {
        let mut choices = Choices::new(NOW, Subject::Command);
        for c in search_text.chars() {
            choices.append(c);
        }
        for (key, begin, exit) in rows {
            choices.add(key.clone(), *begin, *exit);
        }
        choices.recompute_top_items();
        choices
    }

    #[test]
    fn weighs_an_occurrence_by_how_it_exited() {
        for (exit, expected_usage) in [(Some(0), 2.0), (Some(1), 1.0), (None, 0.5)] {
            let choices = choices_for("", &[(String::from("ls"), NOW, exit)]);
            assert_eq!(
                choices.top_items,
                vec![Choice {
                    fuzziness: 0,
                    usage: expected_usage,
                    key: String::from("ls"),
                }]
            );
        }
    }

    #[test]
    fn halves_the_weight_of_an_occurrence_every_half_life() {
        let choices = choices_for(
            "",
            &[
                (String::from("fresh"), NOW, Some(0)),
                (String::from("stale"), NOW - HALF_LIFE, Some(0)),
                (String::from("staler"), NOW - 2 * HALF_LIFE, Some(0)),
            ],
        );

        let use_of: Vec<(&str, f64)> = choices
            .top_items()
            .iter()
            .map(|c| (c.key.as_str(), c.usage))
            .collect();
        assert_eq!(
            use_of,
            vec![("fresh", 2.0), ("stale", 1.0), ("staler", 0.5)]
        );
    }

    #[test]
    fn accumulates_the_use_of_repeated_commands_into_one_item() {
        let choices = choices_for(
            "",
            &[
                (String::from("ls"), NOW, Some(0)),
                (String::from("ls"), NOW, Some(1)),
            ],
        );
        assert_eq!(
            choices.top_items,
            vec![Choice {
                fuzziness: 0,
                usage: 3.0,
                key: String::from("ls"),
            }]
        );
    }

    #[test]
    fn counts_a_tab_completed_command_as_the_one_that_was_typed_out() {
        let choices = choices_for(
            "",
            &[
                (String::from("ls"), NOW, Some(0)),
                (String::from("ls "), NOW, Some(0)),
                (String::from("ls\t"), NOW, Some(0)),
            ],
        );

        assert_eq!(
            choices.top_items(),
            [Choice {
                fuzziness: 0,
                usage: 6.0,
                key: String::from("ls"),
            }]
        );
    }

    #[test]
    fn keeps_the_whitespace_a_command_starts_with() {
        let choices = choices_for("", &[(String::from("  ls"), DAY, Some(0))]);

        assert_eq!(choices.top_items()[0].key, "  ls");
    }

    #[test]
    fn leaves_out_commands_that_do_not_match_the_search_text() {
        let choices = choices_for("zzz", &[(String::from("ls"), DAY, Some(0))]);
        assert_eq!(choices.top_items, vec![]);
    }

    /// A clock that has moved backwards must not cost the commands that were
    /// gathered before it did.
    #[test]
    fn counts_a_command_from_the_future_as_one_from_just_now() {
        let mut choices = Choices::new(NOW, Subject::Command);
        choices.add(String::from("ls"), NOW + DAY, Some(0));
        choices.recompute_top_items();
        assert_eq!(
            choices.top_items,
            vec![Choice {
                fuzziness: 0,
                usage: 2.0,
                key: String::from("ls"),
            }]
        );
    }

    /// Only uses are ever compared against each other, so moving the moment
    /// the picker opened, which multiplies every one of them by the same
    /// amount, must leave the order alone.
    #[test]
    fn ranks_the_same_however_much_later_the_picker_was_opened() {
        let rows = [
            (String::from("cabal build"), NOW - HALF_LIFE, Some(0)),
            (String::from("cargo build"), NOW - 2 * HALF_LIFE, Some(0)),
            (String::from("cargo build"), NOW - 3 * HALF_LIFE, Some(0)),
            (String::from("carry on"), NOW - HALF_LIFE, Some(1)),
        ];
        let mut later = Choices::new(NOW + 7 * HALF_LIFE, Subject::Command);
        for c in "car".chars() {
            later.append(c);
        }
        for (key, begin, exit) in &rows {
            later.add(key.clone(), *begin, *exit);
        }
        later.recompute_top_items();

        let earlier = choices_for("car", &rows);
        let earlier_keys: Vec<&str> = earlier.top_items().iter().map(|c| c.key.as_str()).collect();
        let later_keys: Vec<&str> = later.top_items().iter().map(|c| c.key.as_str()).collect();
        assert_eq!(earlier_keys, later_keys);
    }

    #[test]
    fn ranks_a_better_match_above_a_more_used_one() {
        let choices = choices_for(
            "ls",
            &[
                (String::from("ls"), DAY, Some(0)),
                (String::from("ls"), DAY, Some(0)),
                (String::from("lets"), DAY, Some(0)),
            ],
        );

        let ranking: Vec<&str> = choices.top_items.iter().map(|c| c.key.as_str()).collect();
        assert_eq!(ranking, vec!["ls", "lets"]);
        assert!(choices.top_items[0].fuzziness > choices.top_items[1].fuzziness);
        assert!(choices.top_items[0].usage > choices.top_items[1].usage);
    }

    #[test]
    fn ranks_a_more_used_command_first_among_equally_good_matches() {
        let choices = choices_for(
            "",
            &[
                (String::from("rare"), DAY, Some(0)),
                (String::from("common"), DAY, Some(0)),
                (String::from("common"), DAY, Some(0)),
            ],
        );

        let ranking: Vec<&str> = choices.top_items.iter().map(|c| c.key.as_str()).collect();
        assert_eq!(ranking, vec!["common", "rare"]);
    }

    /// A command is recognised by how it starts, but a directory is not: the
    /// front of a path is boilerplate shared by thousands of them.  Matching
    /// early in the string must therefore not outrank being used, or every
    /// directory under /nix/store outranks every project directory for the
    /// search text "nix".
    #[test]
    fn ranks_directories_by_use_rather_than_by_where_the_match_starts() {
        let store = "/nix/store/lhznlqkn5l53nbsp72fqylhbwkh4qzwn-vmtest-ccr2004";
        let project = "/home/syd/src/nix-ci";
        let mut choices = Choices::new(NOW, Subject::Directory);
        for c in "nix".chars() {
            choices.append(c);
        }
        choices.add(String::from(store), DAY, Some(0));
        for _ in 0..5 {
            choices.add(String::from(project), DAY, Some(0));
        }
        choices.recompute_top_items();

        let ranking: Vec<&str> = choices.top_items().iter().map(|c| c.key.as_str()).collect();
        assert_eq!(ranking, vec![project, store]);
    }

    #[test]
    fn keeps_at_most_max_items_top_items() {
        let rows: Vec<Row> = (0..(MAX_ITEMS * 2))
            .map(|i| (format!("command {i}"), DAY, Some(0)))
            .collect();
        let choices = choices_for("", &rows);
        assert_eq!(choices.top_items.len(), MAX_ITEMS);
    }

    #[test]
    fn narrows_and_widens_without_reading_the_commands_again() {
        let rows = [
            (String::from("cabal build"), DAY, Some(0)),
            (String::from("cargo build"), DAY, Some(0)),
        ];
        let mut choices = Choices::new(NOW, Subject::Command);
        for (key, begin, exit) in &rows {
            choices.add(key.clone(), *begin, *exit);
        }
        choices.recompute_top_items();

        for c in "car".chars() {
            choices.append(c);
        }
        let ranking: Vec<&str> = choices.top_items.iter().map(|c| c.key.as_str()).collect();
        assert_eq!(ranking, vec!["cargo build"]);

        choices.remove();
        choices.remove();
        assert_eq!(choices.top_items.len(), 2);
    }

    #[test]
    fn selects_nothing_in_an_empty_list() {
        assert_eq!(next_selection(None, 0), None);
        assert_eq!(next_selection(Some(0), 0), None);
        assert_eq!(previous_selection(None, 0), None);
        assert_eq!(previous_selection(Some(0), 0), None);
    }

    #[test]
    fn moves_through_the_list_and_wraps_around() {
        assert_eq!(next_selection(Some(0), 3), Some(1));
        assert_eq!(next_selection(Some(2), 3), Some(0));
        assert_eq!(next_selection(None, 3), Some(0));

        assert_eq!(previous_selection(Some(2), 3), Some(1));
        assert_eq!(previous_selection(Some(0), 3), Some(2));
        assert_eq!(previous_selection(None, 3), Some(2));
    }

    #[test]
    fn keeps_the_selection_inside_the_list() {
        assert_eq!(clamped_selection(Some(5), 3), Some(2));
        assert_eq!(clamped_selection(Some(1), 3), Some(1));
        assert_eq!(clamped_selection(Some(1), 0), None);
        assert_eq!(clamped_selection(None, 0), None);
        assert_eq!(clamped_selection(None, 3), Some(0));
    }

    /// The matcher must answer the same way however much it has been used,
    /// which is what lets the list be rebuilt from what has already been read
    /// rather than from a fresh pass.  Its predecessor did not: it kept a
    /// scoring matrix in a thread local and only resized it between calls, so
    /// cells left over from an earlier, larger match were read again and the
    /// same match scored differently depending on what came before it.
    #[test]
    fn the_matcher_answers_the_same_way_however_much_it_has_been_used() {
        let mut choices = Choices::new(NOW, Subject::Command);
        let cold = fuzziness(&mut choices.matcher, &mut choices.haystack, None, "   ");

        let needle = Atom::new(
            "   a",
            CaseMatching::Smart,
            Normalization::Smart,
            AtomKind::Fuzzy,
            false,
        );
        for key in ["   aa", "a much longer command than any of the others here"] {
            let _ = fuzziness(
                &mut choices.matcher,
                &mut choices.haystack,
                Some(&needle),
                key,
            );
        }

        let warm = fuzziness(&mut choices.matcher, &mut choices.haystack, None, "   ");

        assert_eq!(cold, warm);
    }

    fn row() -> impl Strategy<Value = Row> {
        (
            // The long runs space the matched characters far apart, so that
            // faint matches are generated as well as obvious ones.
            prop_oneof!["[a-c ]{1,5}", "ax{20,60}bx{20,60}c"],
            1i64..NOW,
            proptest::option::of(0i64..3i64),
        )
    }

    /// A command whose matched characters lie far apart still matches, and a
    /// faint match is still a match.
    #[test]
    fn shows_a_command_that_matches_only_faintly() {
        let gap = "x".repeat(40);
        let key = format!("a{gap}b{gap}c");

        let choices = choices_for("abc", &[(key.clone(), DAY, Some(0))]);

        assert_eq!(
            choices
                .top_items()
                .iter()
                .map(|c| c.key.as_str())
                .collect::<Vec<&str>>(),
            vec![key.as_str()]
        );
    }

    fn rows() -> impl Strategy<Value = Vec<Row>> {
        prop::collection::vec(row(), 0..50)
    }

    /// Few enough distinct commands that the list holds all of them, so that
    /// the whole result can be compared and not just its first page.
    fn few_rows() -> impl Strategy<Value = Vec<Row>> {
        prop::collection::vec(
            ("[ab]{1,2}", 1i64..NOW, proptest::option::of(0i64..3i64)),
            0..30,
        )
    }

    fn shown(choices: &Choices) -> Vec<(&str, f64)> {
        let mut items: Vec<(&str, f64)> = choices
            .top_items
            .iter()
            .map(|c| (c.key.as_str(), c.usage))
            .collect();
        items.sort_unstable_by(|a, b| a.0.cmp(b.0));
        items
    }

    proptest! {
        /// Typing a character must leave exactly the list that searching for
        /// the longer text from the start would have produced.  This is what
        /// makes it safe not to read the database again.
        #[test]
        fn appending_a_character_matches_searching_for_the_longer_text(
            search_text in "[a-c ]{0,3}",
            c in "[a-c ]",
            rows in rows(),
        ) {
            let c = c.chars().next().unwrap();
            let mut typed = choices_for(&search_text, &rows);
            typed.append(c);

            let mut longer = search_text.clone();
            longer.push(c);
            let from_scratch = choices_for(&longer, &rows);

            prop_assert_eq!(typed.matching_keys(), from_scratch.matching_keys());
            prop_assert_eq!(typed.top_items(), from_scratch.top_items());
            prop_assert_eq!(typed.top_items(), from_scratch.top_items());
        }

        /// However the search text was arrived at, the list must be the same.
        /// Typing, and backing out of, a detour must leave no trace at all.
        #[test]
        fn the_matching_commands_do_not_depend_on_how_the_search_was_typed(
            search_text in "[a-c ]{0,3}",
            detour in "[a-c ]{1,3}",
            rows in rows(),
        ) {
            let direct = choices_for(&search_text, &rows);

            let mut wandered = choices_for(&search_text, &rows);
            for c in detour.chars() {
                wandered.append(c);
            }
            for _ in detour.chars() {
                wandered.remove();
            }

            prop_assert_eq!(wandered.matching_keys(), direct.matching_keys());
            prop_assert_eq!(wandered.top_items(), direct.top_items());
            prop_assert_eq!(wandered.top_items(), direct.top_items());
        }

        /// Backspace must likewise produce exactly the list that searching for
        /// the shorter text from the start would have produced.
        #[test]
        fn removing_a_character_matches_searching_for_the_shorter_text(
            search_text in "[a-c ]{1,4}",
            rows in rows(),
        ) {
            let mut typed = choices_for(&search_text, &rows);
            typed.remove();

            let mut shorter = search_text.clone();
            shorter.pop();
            let from_scratch = choices_for(&shorter, &rows);

            prop_assert_eq!(typed.matching_keys(), from_scratch.matching_keys());
            prop_assert_eq!(typed.top_items(), from_scratch.top_items());
            prop_assert_eq!(typed.top_items(), from_scratch.top_items());
        }

        /// Editing the search text must not disturb the use, which is what
        /// lets them be added up once and kept.
        #[test]
        fn editing_the_search_text_leaves_the_use_alone(
            search_text in "[ab]{0,2}",
            c in "[ab]",
            rows in few_rows(),
        ) {
            let c = c.chars().next().unwrap();
            let mut edited = choices_for(&search_text, &rows);
            edited.append(c);
            edited.remove();

            let from_scratch = choices_for(&search_text, &rows);
            prop_assert_eq!(shown(&edited), shown(&from_scratch));
        }

        /// The result must not depend on how the occurrences were divided into
        /// batches, because that is decided by how much time a frame had left.
        #[test]
        fn the_result_does_not_depend_on_the_batching(
            search_text in "[ab]{0,2}",
            rows in few_rows(),
            batch_size in 1usize..10,
        ) {
            let mut in_batches = Choices::new(NOW, Subject::Command);
            for c in search_text.chars() {
                in_batches.append(c);
            }
            for batch in rows.chunks(batch_size) {
                for (key, begin, exit) in batch {
                    in_batches.add(key.clone(), *begin, *exit);
                }
                in_batches.recompute_top_items();
            }
            in_batches.recompute_top_items();

            let from_scratch = choices_for(&search_text, &rows);
            prop_assert_eq!(shown(&in_batches), shown(&from_scratch));
        }

        #[test]
        fn the_top_items_are_ranked_and_bounded(
            search_text in "[a-c ]{0,3}",
            rows in rows(),
        ) {
            let choices = choices_for(&search_text, &rows);

            prop_assert!(choices.top_items.len() <= MAX_ITEMS);
            // Every occurrence is worth something, and no more than its exit
            // weight, whatever its timestamp.
            for item in &choices.top_items {
                prop_assert!(item.usage > 0.0);
                prop_assert!(item.usage.is_finite());
            }
            for pair in choices.top_items.windows(2) {
                let better = (pair[0].fuzziness, OrderedFloat(pair[0].usage));
                let worse = (pair[1].fuzziness, OrderedFloat(pair[1].usage));
                prop_assert!(better >= worse);
            }
        }

        /// Every command must be shown, as long as there is room for it, and
        /// commands that differ only in trailing whitespace are one command.
        #[test]
        fn shows_every_command_while_there_is_room(
            rows in prop::collection::vec(row(), 0..MAX_ITEMS),
        ) {
            let choices = choices_for("", &rows);

            let mut expected: Vec<&str> = rows.iter().map(|(key, _, _)| key.trim_end()).collect();
            expected.sort_unstable();
            expected.dedup();

            let mut shown: Vec<&str> = choices.top_items.iter().map(|c| c.key.as_str()).collect();
            shown.sort_unstable();

            prop_assert_eq!(shown, expected);
        }
    }
}
