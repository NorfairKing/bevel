module Bevel.CLI.ScoreSpec (spec) where

import Bevel.CLI.Score
import Data.GenValidity.Time ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  describe "scoreFor" $
    it "gives a higher score to more recent times" $
      forAllValid $ \now ->
        forAllValid $ \time1 ->
          forAll (genValid `suchThat` (>= time1)) $ \time2 ->
            let score1 = scoreFor now time1
                score2 = scoreFor now time2
             in shouldSatisfyNamed score2 (">= " <> show score1) (>= score1)
