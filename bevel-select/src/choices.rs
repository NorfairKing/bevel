use fuzzy_matcher::skim::SkimMatcherV2;
use fuzzy_matcher::FuzzyMatcher;
use ordered_float::OrderedFloat;
use std::{cmp::Ordering, collections::HashMap};

const NANOSECONDS_IN_A_DAY: f64 = 86_400_000_000_000_f64;
const MAX_ITEMS: usize = 20;

#[derive(Debug, Clone, PartialEq)]
pub struct Choice {
    pub fuzziness: i64,
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
    fuzziness: i64,
}

pub struct Choices {
    now: i64,
    search_text: String,
    matcher: SkimMatcherV2,
    entries: Vec<Entry>,
    entry_indices: HashMap<String, usize>,
    candidates: Vec<Candidate>,
    top_items: Vec<Choice>,
}

impl Choices {
    pub fn new(now: i64) -> Self {
        Choices {
            now,
            search_text: String::new(),
            matcher: SkimMatcherV2::default(),
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
                let fuzziness = self.fuzziness(&key);
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
        // A command that does not contain the search text as a subsequence
        // cannot contain a longer one either, so adding a character can only
        // take matches away and the commands that already fail to match need
        // not be looked at again.
        let mut candidates = std::mem::take(&mut self.candidates);
        candidates.retain_mut(|candidate| {
            match self.fuzziness(&self.entries[candidate.entry].key) {
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
            // Removing a character can bring matches back, so every command has
            // to be considered again.
            let candidates: Vec<Candidate> = self
                .entries
                .iter()
                .enumerate()
                .filter_map(|(index, entry)| {
                    self.fuzziness(&entry.key).map(|fuzziness| Candidate {
                        entry: index,
                        fuzziness,
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

    /// How well a command matches the search text, or `None` when it does not
    /// match at all.
    ///
    /// Whether this is `None` is decided by looking for the search text as a
    /// subsequence of the command, which depends on nothing else.  The number
    /// that comes back when it does match also depends on which commands were
    /// matched before it, so it settles the order of the list but never who is
    /// in it.  See `the_matcher_scores_the_same_match_differently_over_time`.
    fn fuzziness(&self, key: &str) -> Option<i64> {
        if self.search_text.is_empty() {
            return Some(1);
        }
        self.matcher.fuzzy_match(key, &self.search_text)
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
                    fuzziness: 1,
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
                fuzziness: 1,
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

    /// The matcher is not a pure function: it keeps a scoring matrix in a
    /// thread local and only resizes it between calls, so cells left over from
    /// an earlier, larger match are read again. That is why the properties
    /// below compare which commands match, and not how well they match.
    #[test]
    fn the_matcher_scores_the_same_match_differently_over_time() {
        let matcher = SkimMatcherV2::default();
        let cold = matcher.fuzzy_match("   ", "   ");
        let _ = matcher.fuzzy_match("   aa", "   a");
        let warm = matcher.fuzzy_match("   ", "   ");

        assert_eq!(cold, Some(79));
        assert_eq!(warm, Some(87));
    }

    #[test]
    fn the_matcher_decides_the_same_way_over_time() {
        let matcher = SkimMatcherV2::default();
        let cold = matcher.fuzzy_match("   ", "   a").is_some();
        let _ = matcher.fuzzy_match("   aa", "   a");
        let warm = matcher.fuzzy_match("   ", "   a").is_some();

        assert_eq!(cold, warm);
    }

    fn row() -> impl Strategy<Value = Row> {
        (
            // The long runs matter: they space the matched characters far
            // enough apart that the matcher scores a real match at or below
            // zero, which is where filtering on the score went wrong.
            prop_oneof!["[a-c ]{1,5}", "ax{20,60}bx{20,60}c"],
            1i64..(2 * DAY),
            proptest::option::of(0i64..3i64),
        )
    }

    /// A command that the matcher matches, but only faintly enough to score
    /// below zero from a cold matcher.
    #[test]
    fn shows_a_command_that_matches_only_faintly() {
        let gap = "x".repeat(40);
        let key = format!("a{gap}b{gap}c");
        assert_eq!(SkimMatcherV2::default().fuzzy_match(&key, "abc"), Some(-23));

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
        /// Typing a character must leave exactly the commands that searching
        /// for the longer text from the start would have found. This is what
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
        }

        /// However the search text was arrived at, the commands that match it
        /// must be the same ones.  Typing, and backing out of, a detour must
        /// leave no trace.
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
        }

        /// Backspace must likewise find exactly what searching for the shorter
        /// text from the start would have found.
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
