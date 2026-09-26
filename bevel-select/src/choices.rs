use nucleo_matcher::pattern::{Atom, AtomKind, CaseMatching, Normalization};
use nucleo_matcher::{Config, Matcher, Utf32Str};
use ordered_float::OrderedFloat;
use std::{cmp::Ordering, collections::HashMap};

const NANOSECONDS_IN_A_DAY: f64 = 86_400_000_000_000_f64;
const MAX_ITEMS: usize = 20;

#[derive(Debug, Clone, PartialEq)]
pub struct Choice {
    pub fuzziness: u32,
    pub score: f64,
    pub key: String,
}

/// One distinct command, with the scores of all its occurrences added up.
///
/// The score does not depend on the search text, which is what lets the
/// database be read only once per run.
struct Entry {
    key: String,
    score: f64,
}

/// An entry that matches the search text, with the fuzziness of that match.
#[derive(Clone, Copy)]
struct Candidate {
    entry: usize,
    fuzziness: u32,
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
    pub fn new(now: i64) -> Self {
        let mut config = Config::DEFAULT;
        // A command is usually recognised by how it starts, so a match at the
        // front of one counts for more.
        config.prefer_prefix = true;
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
        let score = self.score(begin, exit);
        match self.entry_indices.get(&key) {
            Some(&index) => self.entries[index].score += score,
            None => {
                let index = self.entries.len();
                let fuzziness = fuzziness(
                    &mut self.matcher,
                    &mut self.haystack,
                    self.needle.as_ref(),
                    &key,
                );
                self.entry_indices.insert(key.clone(), index);
                self.entries.push(Entry { key, score });
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

    /// Bring the top items back in line with the scores.
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
            if !is_rankable(entries[candidate.entry].score) {
                continue;
            }
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
                    score: entry.score,
                    key: entry.key.clone(),
                }
            })
            .collect();
    }

    fn score(&self, begin: i64, exit: Option<i64>) -> f64 {
        let timediff = (self.now - begin) as f64;
        let exit_multiplier = match exit {
            // If the command is still running or was interrupted, it's less relevant.
            None => 0.5f64,
            // If the command is succesful, it's more relevant.
            Some(0) => 2f64,
            Some(_) => 1f64,
        };
        exit_multiplier * NANOSECONDS_IN_A_DAY / timediff
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

/// A command scores negatively if its timestamp lies in the future, and
/// non-finitely if its timestamp is exactly now. Neither belongs in the list.
fn is_rankable(score: f64) -> bool {
    score > 0.0 && score.is_finite()
}

/// Order the candidates best-first: fuzziest match, then highest score, then by
/// command so that the order never depends on how the occurrences arrived.
fn compare(entries: &[Entry], a: &Candidate, b: &Candidate) -> Ordering {
    let left = &entries[a.entry];
    let right = &entries[b.entry];
    b.fuzziness
        .cmp(&a.fuzziness)
        .then_with(|| OrderedFloat(right.score).cmp(&OrderedFloat(left.score)))
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
    const NOW: i64 = 2 * DAY;

    type Row = (String, i64, Option<i64>);

    fn choices_for(search_text: &str, rows: &[Row]) -> Choices {
        let mut choices = Choices::new(NOW);
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
    fn scores_a_command_from_a_day_ago_by_its_exit_multiplier() {
        for (exit, expected_score) in [(Some(0), 2.0), (Some(1), 1.0), (None, 0.5)] {
            let choices = choices_for("", &[(String::from("ls"), DAY, exit)]);
            assert_eq!(
                choices.top_items,
                vec![Choice {
                    fuzziness: 0,
                    score: expected_score,
                    key: String::from("ls"),
                }]
            );
        }
    }

    #[test]
    fn accumulates_the_scores_of_repeated_commands_into_one_item() {
        let choices = choices_for(
            "",
            &[
                (String::from("ls"), DAY, Some(0)),
                (String::from("ls"), DAY, Some(1)),
            ],
        );
        assert_eq!(
            choices.top_items,
            vec![Choice {
                fuzziness: 0,
                score: 3.0,
                key: String::from("ls"),
            }]
        );
    }

    #[test]
    fn leaves_out_commands_that_do_not_match_the_search_text() {
        let choices = choices_for("zzz", &[(String::from("ls"), DAY, Some(0))]);
        assert_eq!(choices.top_items, vec![]);
    }

    #[test]
    fn leaves_out_commands_from_the_future() {
        let mut choices = Choices::new(DAY);
        choices.add(String::from("ls"), 2 * DAY, Some(0));
        choices.recompute_top_items();
        assert_eq!(choices.top_items, vec![]);
    }

    #[test]
    fn leaves_out_commands_from_exactly_now() {
        let mut choices = Choices::new(DAY);
        choices.add(String::from("ls"), DAY, Some(0));
        choices.recompute_top_items();
        assert_eq!(choices.top_items, vec![]);
    }

    #[test]
    fn ranks_a_fuzzier_match_above_a_better_scoring_one() {
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
        assert!(choices.top_items[0].score > choices.top_items[1].score);
    }

    #[test]
    fn ranks_a_higher_scoring_command_first_among_equally_fuzzy_matches() {
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
        let mut choices = Choices::new(NOW);
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
        let mut choices = Choices::new(NOW);
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
            1i64..(2 * DAY),
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
            (
                "[ab]{1,2}",
                1i64..(2 * DAY),
                proptest::option::of(0i64..3i64),
            ),
            0..30,
        )
    }

    fn shown(choices: &Choices) -> Vec<(&str, f64)> {
        let mut items: Vec<(&str, f64)> = choices
            .top_items
            .iter()
            .map(|c| (c.key.as_str(), c.score))
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

        /// Editing the search text must not disturb the scores, which is what
        /// lets them be added up once and kept.
        #[test]
        fn editing_the_search_text_leaves_the_scores_alone(
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
            let mut in_batches = Choices::new(NOW);
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
            for item in &choices.top_items {
                prop_assert!(is_rankable(item.score));
            }
            for pair in choices.top_items.windows(2) {
                let better = (pair[0].fuzziness, OrderedFloat(pair[0].score));
                let worse = (pair[1].fuzziness, OrderedFloat(pair[1].score));
                prop_assert!(better >= worse);
            }
        }

        /// Every command must be shown, as long as there is room for it.
        #[test]
        fn shows_every_command_while_there_is_room(
            rows in prop::collection::vec(row(), 0..MAX_ITEMS),
        ) {
            let choices = choices_for("", &rows);

            let mut expected: Vec<&str> = rows.iter().map(|(key, _, _)| key.as_str()).collect();
            expected.sort_unstable();
            expected.dedup();

            let mut shown: Vec<&str> = choices.top_items.iter().map(|c| c.key.as_str()).collect();
            shown.sort_unstable();

            prop_assert_eq!(shown, expected);
        }
    }
}
