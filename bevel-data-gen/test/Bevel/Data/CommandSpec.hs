{-# LANGUAGE TypeApplications #-}

module Bevel.Data.CommandSpec
  ( spec,
  )
where

import Bevel.Data.Command
import Bevel.Data.Command.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Command
  jsonSpec @Command
