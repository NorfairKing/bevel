module Bevel.CLI.ScoreSpec (spec) where

import Bevel.CLI.Score
import Data.GenValidity.Time ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  describe "scoreFor" $ do
    it "gives a positive score" $
      forAllValid $ \now ->
        forAllValid $ \time ->
          let score = scoreFor now time
           in score >= Score 0
    it "gives a higher score to more recent times" $
      forAllValid $ \now ->
        forAll (genValid `suchThat` (< now)) $ \time1 ->
          forAll (genValid `suchThat` (<= time1)) $ \time2 ->
            let score1 = scoreFor now time1
                score2 = scoreFor now time2
             in shouldSatisfyNamed score2 ("<= " <> show score1) (<= score1)
