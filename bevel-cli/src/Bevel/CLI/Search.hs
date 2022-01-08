{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bevel.CLI.Search where

import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

fuzzySearch :: Text -> Text -> Double
fuzzySearch query option =
  if T.null query
    then 1
    else windowScores maxNeedle query option
  where
    maxNeedle = min (T.length query) (T.length option)

windowScores :: Int -> Text -> Text -> Double
windowScores maxNeedleLength query option = go [1 .. maxNeedleLength]
  where
    go :: [Int] -> Double
    go = \case
      [] -> 0
      (i : is) ->
        let s = windowScore i query option
         in if s <= 0
              then s
              else s + go is

windowScore :: Int -> Text -> Text -> Double
windowScore i query option =
  fromIntegral (i * i)
    * fromIntegral
      ( S.size
          ( S.intersection
              (S.fromList (windows i option))
              (S.fromList (windows i query))
          )
      )

windows :: Int -> Text -> [Text]
windows i = go
  where
    go :: Text -> [Text]
    go t
      | T.length t <= i = [t]
      | otherwise = T.take i t : go (T.drop 1 t)
