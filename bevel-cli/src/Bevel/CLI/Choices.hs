{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bevel.CLI.Choices where

import Bevel.CLI.Score
import Bevel.CLI.Search
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Data.Word

data Choices = Choices
  { choicesTotal :: !Word64,
    choicesMap :: !(Map Text (Double, Score)) -- Fuzziness & score
  }
  deriving (Show)

makeChoices :: UTCTime -> Text -> [(Word64, Text)] -> Choices
makeChoices now query cs =
  Choices
    { choicesTotal = fromIntegral $ length cs,
      choicesMap =
        scoreMap now $
          mapMaybe
            ( \(time, option) ->
                let fuzziness = fuzzySearch query option
                 in if fuzziness > 0 then Just (time, option, fuzziness) else Nothing
            )
            cs
    }

lookupChoice :: Choices -> Text -> (Double, Score)
lookupChoice Choices {..} a = fromMaybe (0, mempty) $ M.lookup a choicesMap

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
