{-# LANGUAGE TypeApplications #-}

module Bevel.CLI.OptParseSpec (spec) where

import Bevel.CLI.OptParse
import OptEnvConf.Test
import Test.Syd

spec :: Spec
spec = do
  describe "Instructions" $ do
    settingsLintSpec @Instructions
    goldenSettingsReferenceDocumentationSpec @Instructions "test_resources/documentation.txt" "bevel"
    goldenSettingsNixOptionsSpec @Instructions "options.nix"
