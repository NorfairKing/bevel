{-# LANGUAGE OverloadedStrings #-}

module Bevel.CLI.SearchSpec (spec) where

import Bevel.CLI.Search
import Data.GenValidity.Text ()
import qualified Data.Text as T
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  describe "fuzzySearch" $ do
    it "matches exact matches" $
      forAllValid $ \t ->
        fuzzySearch t t `shouldBe` True
    it "matches prefixes" $
      forAllValid $ \t1 ->
        forAllValid $ \t2 ->
          fuzzySearch t1 (t1 <> t2) `shouldBe` True
    it "matches suffixes" $
      forAllValid $ \t1 ->
        forAllValid $ \t2 ->
          fuzzySearch t2 (t1 <> t2) `shouldBe` True
    it "matches infixes" $
      forAllValid $ \t1 ->
        forAllValid $ \t2 ->
          forAllValid $ \t3 ->
            fuzzySearch t2 (t1 <> t2 <> t3) `shouldBe` True
    it "matches infixes with something inbetween" $
      forAllValid $ \t1 ->
        forAllValid $ \t2 ->
          forAllValid $ \t3 ->
            forAllValid $ \t4 ->
              fuzzySearch t2 (t1 <> T.take 2 t2 <> t3 <> T.drop 2 t2 <> t4) `shouldBe` True
    it "matches these examples" $ do
      fuzzySearch "foo" "foobar" `shouldBe` True
