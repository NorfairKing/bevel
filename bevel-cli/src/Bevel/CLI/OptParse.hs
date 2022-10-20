{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bevel.CLI.OptParse
  ( getInstructions,
    Instructions (..),
    Dispatch (..),
    Settings (..),
    getDefaultClientDatabase,
    getDefaultDataDir,
    getDefaultConfigFile,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Bevel.API.Server.Data
import Control.Applicative
import Control.Arrow
import Control.Monad.Logger
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Data.Yaml (FromJSON, ToJSON)
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO
import Servant.Client

data Instructions
  = Instructions !Dispatch !Settings
  deriving (Show, Eq, Generic)

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

-- | A product type for the settings that are common across commands
data Settings = Settings
  { settingBaseUrl :: !(Maybe BaseUrl),
    settingUsername :: !(Maybe Username),
    settingPassword :: !(Maybe Text),
    settingMaxOptions :: !Word8,
    settingDbFile :: !(Path Abs File),
    settingLogLevel :: !LogLevel
  }
  deriving (Show, Eq, Generic)

-- | A sum type for the commands and their specific settings
data Dispatch
  = DispatchRegister
  | DispatchLogin
  | DispatchSync
  | DispatchChangeDir
  | DispatchRepeat
  | DispatchRepeatLocal
  | DispatchLast
  deriving (Show, Eq, Generic)

combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments cmd Flags {..}) Environment {..} mConf = do
  -- This is a typical way to combine a setting.
  --
  -- We choose the first of the supplied flag, environment variable or configuration field,
  -- or default value if none of the those were supplied.
  let settingBaseUrl = flagBaseUrl <|> envBaseUrl <|> mc configBaseUrl
  let settingUsername = flagUsername <|> envUsername <|> mc configUsername
  let settingPassword = flagPassword <|> envPassword <|> mc configPassword
  let settingMaxOptions = fromMaybe 15 $ flagMaxOptions <|> envMaxOptions <|> mc configMaxOptions
  settingDbFile <- case flagDbFile <|> envDbFile <|> mc configDbFile of
    Nothing -> getDefaultClientDatabase
    Just dbf -> resolveFile' dbf
  let settingLogLevel = fromMaybe LevelWarn $ flagLogLevel <|> envLogLevel <|> mc configLogLevel
  let sets = Settings {..}
  disp <-
    -- Resolve the command-specific settings for each command
    case cmd of
      CommandRegister -> pure DispatchRegister
      CommandLogin -> pure DispatchLogin
      CommandSync -> pure DispatchSync
      CommandChangeDir -> pure DispatchChangeDir
      CommandRepeat -> pure DispatchRepeat
      CommandRepeatLocal -> pure DispatchRepeatLocal
      CommandLast -> pure DispatchLast
  pure $ Instructions disp sets
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

getDefaultClientDatabase :: IO (Path Abs File)
getDefaultClientDatabase = do
  dataDir <- getDefaultDataDir
  resolveFile dataDir "history.sqlite3"

getDefaultDataDir :: IO (Path Abs Dir)
getDefaultDataDir = getXdgDir XdgData (Just [reldir|bevel|])

getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = do
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|bevel|])
  resolveFile xdgConfigDir "config.yaml"

data Configuration = Configuration
  { configBaseUrl :: !(Maybe BaseUrl),
    configUsername :: !(Maybe Username),
    configPassword :: !(Maybe Text),
    configMaxOptions :: !(Maybe Word8),
    configDbFile :: !(Maybe FilePath),
    configLogLevel :: !(Maybe LogLevel)
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldWith "server-url" (bimapCodec (left show . parseBaseUrl) showBaseUrl codec) "Server base url" .= configBaseUrl
        <*> optionalField "username" "Server account username" .= configUsername
        <*> optionalField "password" "Server account password" .= configPassword
        <*> optionalField "max-options" "Maximum number of options to show when selecting a previous command or directory" .= configMaxOptions
        <*> optionalField "database" "The path to the database" .= configDbFile
        <*> optionalField "log-level" "The minimal severity for log messages" .= configLogLevel

instance HasCodec LogLevel where
  codec =
    stringConstCodec
      [ (LevelDebug, "Debug"),
        (LevelInfo, "Info"),
        (LevelWarn, "Warn"),
        (LevelError, "Error")
      ]

-- | Get the configuration
--
-- We use the flags and environment because they can contain information to override where to look for the configuration files.
-- We return a 'Maybe' because there may not be a configuration file.
getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> getDefaultConfigFile >>= readYamlConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

-- | What we find in the configuration variable.
--
-- Do nothing clever here, just represent the relevant parts of the environment.
-- For example, use 'Text', not 'SqliteConfig'.
data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envBaseUrl :: !(Maybe BaseUrl),
    envUsername :: !(Maybe Username),
    envPassword :: !(Maybe Text),
    envMaxOptions :: !(Maybe Word8),
    envDbFile :: !(Maybe FilePath),
    envLogLevel :: !(Maybe LogLevel)
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

-- | The 'envparse' parser for the 'Environment'
environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "BEVEL_" $
    Environment
      <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "Config file"))
      <*> optional (Env.var (maybe (Left $ Env.unread "unable to parse base url") Right . parseBaseUrl) "SERVER_URL" (Env.help "Server base url"))
      <*> optional (Env.var (left Env.unread . parseUsernameOrErr . T.pack) "USERNAME" (Env.help "Server account username"))
      <*> optional (Env.var Env.str "PASSWORD" (Env.help "Server account password"))
      <*> optional (Env.var Env.auto "MAX_OPTIONS" (Env.help "Maximum number of options to show when selecting a previous command or directory"))
      <*> optional (Env.var Env.str "DATABASE" (Env.help "Path to the database"))
      <*> optional (Env.var Env.auto "LOG_LEVEL" (Env.help "Minimal severity for log messages"))

-- | The combination of a command with its specific flags and the flags for all commands
data Arguments
  = Arguments !Command !Flags
  deriving (Show, Eq, Generic)

-- | Get the command-line arguments
getArguments :: IO Arguments
getArguments = customExecParser prefs_ argParser

-- | The 'optparse-applicative' parsing preferences
prefs_ :: OptParse.ParserPrefs
prefs_ =
  -- I like these preferences. Use what you like.
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

-- | The @optparse-applicative@ parser for 'Arguments'
argParser :: OptParse.ParserInfo Arguments
argParser =
  OptParse.info
    (OptParse.helper <*> parseArgs)
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    -- Show the variables from the environment that we parse and the config file format
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (renderColouredSchemaViaCodec @Configuration)
        ]

parseArgs :: OptParse.Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

-- | A sum type for the commands and their specific arguments
data Command
  = CommandRegister
  | CommandLogin
  | CommandSync
  | CommandChangeDir
  | CommandRepeat
  | CommandRepeatLocal
  | CommandLast
  deriving (Show, Eq, Generic)

parseCommand :: OptParse.Parser Command
parseCommand =
  OptParse.hsubparser $
    mconcat
      [ OptParse.command "register" parseCommandRegister,
        OptParse.command "login" parseCommandLogin,
        OptParse.command "sync" parseCommandSync,
        OptParse.command "cd" parseCommandChangeDir,
        OptParse.command "repeat" parseCommandRepeat,
        OptParse.command "repeat-local" parseCommandRepeatLocal,
        OptParse.command "last" parseCommandLast
      ]

parseCommandRegister :: OptParse.ParserInfo Command
parseCommandRegister = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Register the user"
    parser = pure CommandRegister

parseCommandLogin :: OptParse.ParserInfo Command
parseCommandLogin = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Log the user in"
    parser = pure CommandLogin

parseCommandSync :: OptParse.ParserInfo Command
parseCommandSync = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Synchronise the thing database"
    parser = pure CommandSync

parseCommandChangeDir :: OptParse.ParserInfo Command
parseCommandChangeDir = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Select a directory to change to, based on terminal history"
    parser = pure CommandChangeDir

parseCommandRepeat :: OptParse.ParserInfo Command
parseCommandRepeat = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Select a command to run again"
    parser = pure CommandRepeat

parseCommandRepeatLocal :: OptParse.ParserInfo Command
parseCommandRepeatLocal = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Select a command to run again, from the local working directory's history"
    parser = pure CommandRepeatLocal

parseCommandLast :: OptParse.ParserInfo Command
parseCommandLast = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Print the last-visited directory"
    parser = pure CommandLast

-- | The flags that are common across commands.
data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagBaseUrl :: !(Maybe BaseUrl),
    flagUsername :: !(Maybe Username),
    flagPassword :: !(Maybe Text),
    flagMaxOptions :: !(Maybe Word8),
    flagDbFile :: !(Maybe FilePath),
    flagLogLevel :: !(Maybe LogLevel)
  }
  deriving (Show, Eq, Generic)

-- | The 'optparse-applicative' parser for the 'Flags'.
parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "Give the path to an altenative config file",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( option
          (maybeReader parseBaseUrl)
          ( mconcat
              [ long "server-url",
                help "Server base url"
              ]
          )
      )
    <*> optional
      ( option
          (eitherReader $ parseUsernameOrErr . T.pack)
          ( mconcat
              [ long "username",
                help "Server account username",
                metavar "USERNAME"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "password",
                help "Server account password",
                metavar "PASSWORD"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "max-options",
                help "Maximum number of options to show when selecting a previous command or directory",
                metavar "NUMBER"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "database",
                help "Path to the database",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "log-level",
                help "Minimal severity level for log messages",
                metavar "LOG_LEVEL"
              ]
          )
      )
