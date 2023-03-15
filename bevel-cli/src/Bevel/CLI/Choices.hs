{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bevel.CLI.Choices where

import Bevel.CLI.Score
import Bevel.CLI.Search
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Data.Word
import Witherable as Filterable

data Choices = Choices
  { choicesTotal :: !Word64,
    choicesMap :: !(Map Text (Fuzziness, Score))
  }
  deriving (Show)

makeChoices :: (Foldable f, Filterable f) => UTCTime -> Text -> f (Word64, Text) -> Choices
makeChoices now query cs =
  Choices
    { choicesTotal = fromIntegral $ length cs,
      choicesMap =
        scoreMap now $
          Filterable.mapMaybe
            ( \(time, option) ->
                let fuzziness = fuzzySearch query option
                 in if fuzziness > Fuzziness 0
                      then Just (time, option, fuzziness)
                      else Nothing
            )
            cs
    }

lookupChoice :: Choices -> Text -> (Fuzziness, Score)
lookupChoice Choices {..} a = fromMaybe (Fuzziness 0, mempty) $ M.lookup a choicesMap

instance Semigroup Choices where
  (<>) c1 c2 =
    Choices
      { choicesTotal = choicesTotal c1 + choicesTotal c2,
        choicesMap =
          M.unionWith
            (\(f1, s1) (_, s2) -> (f1, s1 <> s2)) -- Combine score but not fuzziness
            (choicesMap c1)
            (choicesMap c2)
      }

instance Monoid Choices where
  mempty =
    Choices
      { choicesTotal = 0,
        choicesMap = M.empty
      }
  mappend = (<>)
