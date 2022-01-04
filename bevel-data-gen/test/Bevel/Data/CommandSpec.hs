{-# LANGUAGE TypeApplications #-}

module Bevel.Data.CommandSpec
  ( spec,
  )
where

import Bevel.Data.Command
import Bevel.Data.Command.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Command
  jsonSpec @Command
