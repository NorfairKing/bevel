{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bevel.CLI.Choices where

import Bevel.CLI.Score
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Data.Word

data Choices = Choices
  { choicesTotal :: !Word64,
    choicesMap :: !(Map Text Double)
  }
  deriving (Show)

makeChoices :: UTCTime -> [(Word64, Text)] -> Choices
makeChoices now cs =
  Choices
    { choicesTotal = fromIntegral $ length cs,
      choicesMap = scoreMap now cs
    }

lookupChoiceScore :: Choices -> Text -> Double
lookupChoiceScore Choices {..} a = fromMaybe 0 $ M.lookup a choicesMap

instance Semigroup Choices where
  (<>) c1 c2 =
    Choices
      { choicesTotal = choicesTotal c1 + choicesTotal c2,
        choicesMap = M.unionWith (+) (choicesMap c1) (choicesMap c2)
      }

instance Monoid Choices where
  mempty =
    Choices
      { choicesTotal = 0,
        choicesMap = M.empty
      }
  mappend = (<>)
