{-# LANGUAGE TypeApplications #-}

module Bevel.API.DataSpec
  ( spec,
  )
where

import Bevel.API.Data
import Bevel.API.Data.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @RegistrationForm
  genValidSpec @LoginForm
  genValidSpec @DownloadRequest
  jsonSpec @DownloadRequest
  genValidSpec @DownloadResponse
  jsonSpec @DownloadResponse
