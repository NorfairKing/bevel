{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Bevel.Client.Data.DB where

import Bevel.API.Server.Data
import Bevel.Data
import Data.Int
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import Data.Word
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics (Generic)
import Path

share
  [mkPersist sqlSettings, mkMigrate "clientMigration"]
  [persistLowerCase|

ClientCommand sql=command
  text Text
  begin Word64
  end Word64 Maybe default=NULL
  workdir (Path Abs Dir)
  user Text
  host Text
  exit Int8 Maybe default=NULL

  serverId ServerCommandId Maybe default=NULL

  deriving Show
  deriving Eq
  deriving Ord
  deriving Generic

|]

instance Validity ClientCommand

clientMakeCommand :: ClientCommand -> Command
clientMakeCommand ClientCommand {..} = Command {..}
  where
    commandText = clientCommandText
    commandBegin = clientCommandBegin
    commandEnd = clientCommandEnd
    commandWorkdir = clientCommandWorkdir
    commandUser = clientCommandUser
    commandHost = clientCommandHost
    commandExit = clientCommandExit

makeSyncedClientCommand :: ServerCommandId -> Command -> ClientCommand
makeSyncedClientCommand sid = makeClientCommand (Just sid)

makeUnsyncedClientCommand :: Command -> ClientCommand
makeUnsyncedClientCommand = makeClientCommand Nothing

makeClientCommand :: Maybe ServerCommandId -> Command -> ClientCommand
makeClientCommand clientCommandServerId Command {..} = ClientCommand {..}
  where
    clientCommandText = commandText
    clientCommandBegin = commandBegin
    clientCommandEnd = commandEnd
    clientCommandWorkdir = commandWorkdir
    clientCommandUser = commandUser
    clientCommandHost = commandHost
    clientCommandExit = commandExit
