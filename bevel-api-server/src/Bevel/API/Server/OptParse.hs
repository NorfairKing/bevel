{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bevel.API.Server.OptParse where

import Autodocodec
import Control.Monad.Logger
import OptEnvConf
import Path
import Paths_bevel_api_server (version)

getSettings :: IO Settings
getSettings = runSettingsParser version "bevel API server"

data Settings = Settings
  { settingPort :: !Int,
    settingLogLevel :: !LogLevel,
    settingDbFile :: !(Path Abs File),
    settingSigningKeyFile :: !(Path Abs File)
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
  settingLogLevel <-
    setting
      [ help "minimal severity of log messages",
        reader $ maybeReader $ \case
          "Debug" -> Just LevelDebug
          "Info" -> Just LevelInfo
          "Warn" -> Just LevelWarn
          "Error" -> Just LevelError
          _ -> Nothing,
        name "log-level",
        value LevelWarn,
        metavar "LOG_LEVEL"
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
  pure Settings {..}

instance HasCodec LogLevel where
  codec =
    stringConstCodec
      [ (LevelDebug, "Debug"),
        (LevelInfo, "Info"),
        (LevelWarn, "Warn"),
        (LevelError, "Error")
      ]
