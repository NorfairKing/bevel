{-# LANGUAGE TypeApplications #-}

module Bevel.API.Server.Data.UsernameSpec
  ( spec,
  )
where

import Bevel.API.Server.Data.Gen ()
import Bevel.API.Server.Data.Username
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = genValidSpec @Username
