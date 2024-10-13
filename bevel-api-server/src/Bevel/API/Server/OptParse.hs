{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bevel.API.Server.OptParse where

import qualified Necrork
import OptEnvConf
import Path
import Paths_bevel_api_server (version)

getSettings :: IO Settings
getSettings = runSettingsParser version "bevel API server"

data Settings = Settings
  { settingPort :: !Int,
    settingDbFile :: !(Path Abs File),
    settingSigningKeyFile :: !(Path Abs File),
    settingNecrorkNotifierSettings :: !(Maybe Necrork.NotifierSettings)
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings = subEnv_ "bevel-api-server" $ withLocalYamlConfig $ do
  settingPort <-
    setting
      [ help "port to serve requests on",
        reader auto,
        name "port",
        metavar "PORT",
        value 8000
      ]
  settingDbFile <-
    filePathSetting
      [ help "database file",
        name "database",
        value "bevel.sqlite"
      ]
  settingSigningKeyFile <-
    filePathSetting
      [ help "signing key file",
        name "signing-key",
        value "signing-key.dat"
      ]
  settingNecrorkNotifierSettings <- optional $ subSettings "necrork"
  pure Settings {..}
