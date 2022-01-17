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

newtype Choices a = Choices
  { choicesMap :: Map a Double
  }
  deriving (Show)

makeChoices :: Ord a => UTCTime -> [(Word64, a)] -> Choices a
makeChoices now = Choices . scoreMap now

lookupChoiceScore :: Ord a => Choices a -> a -> Double
lookupChoiceScore Choices {..} a = fromMaybe 0 $ M.lookup a choicesMap

instance Ord a => Semigroup (Choices a) where
  (<>) (Choices c1) (Choices c2) = Choices $ M.unionWith (+) c1 c2

instance Ord a => Monoid (Choices a) where
  mempty = Choices M.empty
  mappend = (<>)
