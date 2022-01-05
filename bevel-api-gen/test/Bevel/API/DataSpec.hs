{-# LANGUAGE TypeApplications #-}

module Bevel.API.DataSpec
  ( spec,
  )
where

import Bevel.API.Data
import Bevel.API.Data.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @RegistrationForm
  genValidSpec @LoginForm
