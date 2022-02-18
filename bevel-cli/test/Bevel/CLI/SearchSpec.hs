{-# LANGUAGE OverloadedStrings #-}

module Bevel.CLI.SearchSpec (spec) where

import Bevel.CLI.Search
import Data.GenValidity.Text ()
import qualified Data.Text as T
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "fuzzySearch" $ do
    it "matches everything using the empty text" $
      forAllValid $ \t ->
        fuzzySearch "" t `shouldSatisfy` (>= Fuzziness 0)

    it "matches exact matches" $
      forAllValid $ \t ->
        fuzzySearch t t `shouldSatisfy` (>= Fuzziness 0)

    it "matches exact matches more than any other match" $
      forAllValid $ \t ->
        forAllValid $ \t2 ->
          fuzzySearch t t `shouldSatisfy` (>= fuzzySearch t t2)

    it "matches prefixes" $
      forAllValid $ \t1 ->
        forAllValid $ \t2 ->
          fuzzySearch t1 (t1 <> t2) `shouldSatisfy` (>= Fuzziness 0)

    it "matches suffixes" $
      forAllValid $ \t1 ->
        forAllValid $ \t2 ->
          fuzzySearch t2 (t1 <> t2) `shouldSatisfy` (>= Fuzziness 0)

    it "matches infixes" $
      forAllValid $ \t1 ->
        forAllValid $ \t2 ->
          forAllValid $ \t3 ->
            fuzzySearch t2 (t1 <> t2 <> t3) `shouldSatisfy` (>= Fuzziness 0)

    it "matches infixes with something inbetween" $
      forAllValid $ \t1 ->
        forAllValid $ \t2 ->
          forAllValid $ \t3 ->
            forAllValid $ \t4 ->
              fuzzySearch t2 (t1 <> T.take 2 t2 <> t3 <> T.drop 2 t2 <> t4) `shouldSatisfy` (>= Fuzziness 0)

    it "does not care about cases" $ do
      fuzzySearch "abc" "ABC" `shouldSatisfy` (>= Fuzziness 0)

    it "does not match these examples" $ do
      fuzzySearch "abc" "def" `shouldSatisfy` (<= Fuzziness 0)
      fuzzySearch "abcdef" "ghijkl" `shouldSatisfy` (<= Fuzziness 0)

    it "matches this longer match more" $ do
      fuzzySearch "abc" "bcd" `shouldSatisfy` (>= fuzzySearch "abc" "cde")

    it "works on these regression examples" $ do
      fuzzySearch "reb" "nixos-rebuild switch" `shouldSatisfy` (> fuzzySearch "reb" "tree")

    it "matches a double-match more than a single-match" $
      forAllValid $ \t1 ->
        forAllValid $ \t2 ->
          fuzzySearch t1 (t1 <> t2 <> t1) `shouldSatisfy` (>= fuzzySearch t1 (t1 <> t2))

  describe "windows" $ do
    it "works on this single-character example" $
      windows 1 "foobar" `shouldBe` ["f", "o", "o", "b", "a", "r"]
    it "works on this example" $
      windows 2 "foobar" `shouldBe` ["fo", "oo", "ob", "ba", "ar"]
    it "works on this longer" $
      windows 3 "foobarbaz" `shouldBe` ["foo", "oob", "oba", "bar", "arb", "rba", "baz"]
