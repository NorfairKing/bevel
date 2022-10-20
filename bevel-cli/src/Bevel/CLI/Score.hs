{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bevel.CLI.Score where

import Data.Fixed
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import GHC.Generics (Generic)

newtype Score = Score {unScore :: Double}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)
  deriving (Semigroup, Monoid) via (Sum Double)

scoreMap :: forall a b. Ord a => UTCTime -> [(Word64, a, b)] -> Map a (b, Score)
scoreMap now = foldl' go M.empty
  where
    nowNanos = utcTimeToNanos now :: Word64
    go :: Map a (b, Score) -> (Word64, a, b) -> Map a (b, Score)
    go m (time, a, b) = M.alter go' a m
      where
        additionalScore = scoreFor nowNanos time
        go' :: Maybe (b, Score) -> Maybe (b, Score)
        go' = \case
          Nothing -> Just (b, additionalScore)
          Just (_, n) -> Just (b, n <> additionalScore)

utcTimeToNanos :: UTCTime -> Word64
utcTimeToNanos u =
  let MkFixed i = nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds u
   in fromIntegral $ i `div` 1_000

scoreFor :: Word64 -> Word64 -> Score
scoreFor nowNanos time = Score $ 86400_000_000_000 / realToFrac timediff
  where
    timediff = max 1 $ nowNanos - time
