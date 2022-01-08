{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bevel.CLI.Score where

import Data.Fixed
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word

scoreMap :: forall a. Ord a => UTCTime -> [(Word64, a)] -> Map a Double
scoreMap now = foldl' go M.empty
  where
    nowNanos = utcTimeToNanos now :: Word64
    go :: Map a Double -> (Word64, a) -> Map a Double
    go m (time, a) = M.alter go' a m
      where
        additionalScore = scoreFor nowNanos time
        go' :: Maybe Double -> Maybe Double
        go' = \case
          Nothing -> Just additionalScore
          Just n -> Just (n + additionalScore)

utcTimeToNanos :: UTCTime -> Word64
utcTimeToNanos u =
  let MkFixed i = nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds u
   in fromIntegral $ i `div` 1000

scoreFor :: Word64 -> Word64 -> Double
scoreFor nowNanos time = 86400_000_000_000 / realToFrac timediff
  where
    timediff = max 1 $ nowNanos - time
