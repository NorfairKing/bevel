{-# LANGUAGE TypeApplications #-}

module Bevel.API.Server.OptParseSpec (spec) where

import Bevel.API.Server.OptParse
import OptEnvConf.Test
import Test.Syd

spec :: Spec
spec = do
  describe "Settings" $ do
    settingsLintSpec @Settings
    goldenSettingsReferenceDocumentationSpec @Settings "test_resources/documentation.txt" "bevel-api-server"
    goldenSettingsNixOptionsSpec @Settings "options.nix"
