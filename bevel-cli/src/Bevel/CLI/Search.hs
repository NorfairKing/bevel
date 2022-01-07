module Bevel.CLI.Search (fuzzySearch) where

import Data.Text (Text)
import qualified Data.Text as T

fuzzySearch :: Text -> Text -> Bool
fuzzySearch query option = null $ T.foldl' go (T.unpack (T.toCaseFold query)) (T.toCaseFold option)
  where
    go :: [Char] -> Char -> [Char]
    go [] _ = []
    go q@(qc : rest) dc
      | qc == dc = rest
      | otherwise = q
