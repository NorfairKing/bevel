{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Bevel.CLI.Choices where

import Bevel.CLI.Score
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import Data.Word

data Choices a = Choices
  { choicesTotal :: !Word64,
    choicesMap :: !(Map a Double)
  }
  deriving (Show)

makeChoices :: Ord a => UTCTime -> [(Word64, a)] -> Choices a
makeChoices now cs =
  Choices
    { choicesTotal = fromIntegral $ length cs,
      choicesMap = scoreMap now cs
    }

lookupChoiceScore :: Ord a => Choices a -> a -> Double
lookupChoiceScore Choices {..} a = fromMaybe 0 $ M.lookup a choicesMap

instance Ord a => Semigroup (Choices a) where
  (<>) c1 c2 =
    Choices
      { choicesTotal = choicesTotal c1 + choicesTotal c2,
        choicesMap = M.unionWith (+) (choicesMap c1) (choicesMap c2)
      }

instance Ord a => Monoid (Choices a) where
  mempty =
    Choices
      { choicesTotal = 0,
        choicesMap = M.empty
      }
  mappend = (<>)
