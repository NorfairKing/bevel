use fuzzy_matcher::skim::SkimMatcherV2;
use fuzzy_matcher::FuzzyMatcher;
use ordered_float::OrderedFloat;
use std::{cmp::Reverse, collections::HashMap};

const NANOSECONDS_IN_A_DAY: f64 = 86_400_000_000_000_f64;
const MAX_ITEMS: usize = 20;

#[derive(Debug, Clone, PartialEq)]
pub struct Choice {
    pub fuzziness: i64,
    pub score: f64,
    pub key: String,
}

pub struct Choices {
    now: i64,
    pub search_text: String,
    matcher: SkimMatcherV2,
    pub top_items: Vec<Choice>,
    item_scores: HashMap<String, f64>,
}

impl Choices {
    pub fn new(search_text: String, now: i64) -> Self {
        Choices {
            now,
            search_text,
            matcher: SkimMatcherV2::default(),
            top_items: Vec::with_capacity(MAX_ITEMS),
            item_scores: HashMap::new(),
        }
    }

    /// Add one command occurrence, keeping the top items up to date.
    pub fn add(&mut self, key: String, begin: i64, exit: Option<i64>) {
        let fuzziness = self.fuzziness(&key);
        if fuzziness <= 0 {
            return;
        }

        let timediff = (self.now - begin) as f64;
        let exit_multiplier = match exit {
            // If the command is still running or was interrupted, it's less relevant.
            None => 0.5f64,
            // If the command is succesful, it's more relevant.
            Some(0) => 2f64,
            Some(_) => 1f64,
        };
        let score = exit_multiplier * NANOSECONDS_IN_A_DAY / timediff;

        let total_score: f64 = *self
            .item_scores
            .entry(key.clone())
            .and_modify(|s| {
                *s += score;
            })
            .or_insert(score);

        // A command from the future scores negatively, and does not belong in
        // the top items at all.
        if total_score > 0.0 {
            self.top_items.retain(|c| c.key != key);
            self.top_items.push(Choice {
                fuzziness,
                score: total_score,
                key,
            });
            self.top_items
                .sort_by_key(|c| (Reverse(c.fuzziness), Reverse(OrderedFloat(c.score))));
            if self.top_items.len() > MAX_ITEMS {
                self.top_items.pop();
            }
        }
    }

    fn fuzziness(&self, key: &str) -> i64 {
        if self.search_text.is_empty() {
            1
        } else {
            self.matcher
                .fuzzy_match(key, &self.search_text)
                .unwrap_or(0)
        }
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    const DAY: i64 = 86_400_000_000_000;

    #[test]
    fn scores_a_command_from_a_day_ago_by_its_exit_multiplier() {
        for (exit, expected_score) in [(Some(0), 2.0), (Some(1), 1.0), (None, 0.5)] {
            let mut choices = Choices::new(String::new(), 2 * DAY);
            choices.add(String::from("ls"), DAY, exit);
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
        let mut choices = Choices::new(String::new(), 2 * DAY);
        choices.add(String::from("ls"), DAY, Some(0));
        choices.add(String::from("ls"), DAY, Some(1));
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
        let mut choices = Choices::new(String::from("zzz"), 2 * DAY);
        choices.add(String::from("ls"), DAY, Some(0));
        assert_eq!(choices.top_items, vec![]);
    }

    #[test]
    fn leaves_out_commands_from_the_future() {
        let mut choices = Choices::new(String::new(), DAY);
        choices.add(String::from("ls"), 2 * DAY, Some(0));
        assert_eq!(choices.top_items, vec![]);
    }

    #[test]
    fn ranks_a_fuzzier_match_above_a_better_scoring_one() {
        let mut choices = Choices::new(String::from("ls"), 2 * DAY);
        choices.add(String::from("ls"), DAY, Some(0));
        choices.add(String::from("ls"), DAY, Some(0));
        choices.add(String::from("lets"), DAY, Some(0));

        let ranking: Vec<&str> = choices.top_items.iter().map(|c| c.key.as_str()).collect();
        assert_eq!(ranking, vec!["ls", "lets"]);
        assert!(choices.top_items[0].fuzziness > choices.top_items[1].fuzziness);
        assert!(choices.top_items[0].score > choices.top_items[1].score);
    }

    #[test]
    fn ranks_a_higher_scoring_command_first_among_equally_fuzzy_matches() {
        let mut choices = Choices::new(String::new(), 2 * DAY);
        choices.add(String::from("rare"), DAY, Some(0));
        choices.add(String::from("common"), DAY, Some(0));
        choices.add(String::from("common"), DAY, Some(0));

        let ranking: Vec<&str> = choices.top_items.iter().map(|c| c.key.as_str()).collect();
        assert_eq!(ranking, vec!["common", "rare"]);
    }

    #[test]
    fn keeps_at_most_max_items_top_items() {
        let mut choices = Choices::new(String::new(), 2 * DAY);
        for i in 0..(MAX_ITEMS * 2) {
            choices.add(format!("command {i}"), DAY, Some(0));
        }
        assert_eq!(choices.top_items.len(), MAX_ITEMS);
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
}
