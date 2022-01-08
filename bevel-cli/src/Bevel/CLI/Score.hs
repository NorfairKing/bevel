{-# LANGUAGE LambdaCase #-}
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
    go :: Map a Double -> (Word64, a) -> Map a Double
    go m (time, a) = M.alter go' a m
      where
        additionalScore = scoreFor now time
        go' :: Maybe Double -> Maybe Double
        go' = \case
          Nothing -> Just additionalScore
          Just n -> Just (n + additionalScore)

scoreFor :: UTCTime -> Word64 -> Double
scoreFor now time = realToFrac $ nominalDay / timediff
  where
    picoSeconds = MkFixed $ fromIntegral time * 1000 :: Pico
    epochDiffTime = secondsToNominalDiffTime picoSeconds :: NominalDiffTime
    nowInDiffTime = utcTimeToPOSIXSeconds now :: NominalDiffTime
    timediff = max 1 $ nowInDiffTime - epochDiffTime :: NominalDiffTime
