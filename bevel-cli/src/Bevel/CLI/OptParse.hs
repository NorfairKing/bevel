{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bevel.CLI.OptParse
  ( getInstructions,
    Instructions (..),
    Dispatch (..),
    Settings (..),
  )
where

import Autodocodec
import Bevel.API.Server.Data
import Control.Applicative
import Control.Monad.Logger
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import OptEnvConf
import Path
import Path.IO
import Paths_bevel_cli (version)
import Servant.Client

getInstructions :: IO Instructions
getInstructions = runSettingsParser version "bevel command-line interface"

data Instructions
  = Instructions !Dispatch !Settings

instance HasParser Instructions where
  settingsParser = parseInstructions

{-# ANN parseInstructions ("NOCOVER" :: String) #-}
parseInstructions :: Parser Instructions
parseInstructions = do
  subEnv_ "bevel" $
    withConfigurableYamlConfig (xdgYamlConfigFile "bevel") $
      Instructions
        <$> settingsParser
        <*> settingsParser

-- | A product type for the settings that are common across commands
data Settings = Settings
  { settingBaseUrl :: !(Maybe BaseUrl),
    settingUsername :: !(Maybe Username),
    settingPassword :: !(Maybe Text),
    settingMaxOptions :: !Word8,
    settingDbFile :: !(Path Abs File),
    settingLogLevel :: !LogLevel
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings = do
  settingBaseUrl <-
    optional $
      mapIO parseBaseUrl $
        setting
          [ help "server base url",
            reader str,
            name "server-url",
            metavar "URL"
          ]
  settingUsername <-
    optional $
      setting
        [ help "user name",
          reader $ eitherReader (parseUsernameOrErr . T.pack),
          name "username",
          metavar "USERNAME"
        ]
  settingPassword <-
    optional $
      secretTextFileOrBareSetting
        [ help "password",
          name "password",
          metavar "PASSWORD"
        ]
  settingMaxOptions <-
    setting
      [ help "maximum number of options to show when selecting",
        reader auto,
        name "max-options",
        value 15,
        metavar "INT"
      ]
  settingDbFile <-
    choice
      [ filePathSetting
          [ help "path to the database file",
            name "database"
          ],
        runIO getDefaultClientDatabase
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
  pure Settings {..}

getDefaultClientDatabase :: IO (Path Abs File)
getDefaultClientDatabase = do
  dataDir <- getDefaultDataDir
  resolveFile dataDir "history.sqlite3"

getDefaultDataDir :: IO (Path Abs Dir)
getDefaultDataDir = getXdgDir XdgData (Just [reldir|bevel|])

-- | A sum type for the commands and their specific settings
data Dispatch
  = DispatchRegister
  | DispatchLogin
  | DispatchSync
  | DispatchLast

instance HasParser Dispatch where
  settingsParser = parseDispatch

{-# ANN parseDispatch ("NOCOVER" :: String) #-}
parseDispatch :: Parser Dispatch
parseDispatch = do
  commands
    [ command "register" "register an account" $ pure DispatchRegister,
      command "login" "authenticate an account" $ pure DispatchLogin,
      command "sync" "synchronise commands" $ pure DispatchSync,
      command "last" "show the most recent command" $ pure DispatchLast
    ]

instance HasCodec LogLevel where
  codec =
    stringConstCodec
      [ (LevelDebug, "Debug"),
        (LevelInfo, "Info"),
        (LevelWarn, "Warn"),
        (LevelError, "Error")
      ]
