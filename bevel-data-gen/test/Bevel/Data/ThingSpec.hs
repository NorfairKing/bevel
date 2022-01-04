{-# LANGUAGE TypeApplications #-}

module Bevel.Data.ThingSpec
  ( spec,
  )
where

import Bevel.Data.Thing
import Bevel.Data.Thing.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Thing
  jsonSpec @Thing
