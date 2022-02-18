{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bevel.CLI.Search where

import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype Fuzziness = Fuzziness {unFuzziness :: Double}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)

fuzzySearch :: Text -> Text -> Fuzziness
fuzzySearch query option =
  if T.null query
    then Fuzziness 1
    else windowScores maxNeedle query option
  where
    maxNeedle = min (T.length query) (T.length option)

windowScores :: Int -> Text -> Text -> Fuzziness
windowScores maxNeedleLength query option = go [1 .. maxNeedleLength]
  where
    go :: [Int] -> Fuzziness
    go = \case
      [] -> Fuzziness 0
      (i : is) ->
        let Fuzziness s = windowScore i query option
         in Fuzziness $
              if s <= 0
                then s
                else
                  let Fuzziness r = go is
                   in s + r

windowScore :: Int -> Text -> Text -> Fuzziness
windowScore i query option =
  Fuzziness $
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
