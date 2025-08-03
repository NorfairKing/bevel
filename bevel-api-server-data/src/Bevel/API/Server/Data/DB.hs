{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bevel.API.Server.Data.DB where

import Bevel.API.Server.Data.Username
import Bevel.Data
import Data.Int
import Data.Password.Bcrypt
import Data.Password.Instances ()
import Data.Text (Text)
import Data.Validity.Persist ()
import Data.Word
import Database.Esqueleto.Experimental as E
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "automaticServerMigrations"]
  [persistLowerCase|

User
  name Username
  password (PasswordHash Bcrypt)

  UniqueUsername name

  deriving Show
  deriving Eq
  deriving Ord


ServerCommand sql=command
  serverUser UserId

  text Text
  begin Word64
  end Word64 Maybe default=NULL
  workdir Text
  user Text
  host Text
  exit Int8 Maybe default=NULL

  deriving Show
  deriving Eq
  deriving Ord

|]

serverMakeCommand :: ServerCommand -> Command
serverMakeCommand ServerCommand {..} = Command {..}
  where
    commandText = serverCommandText
    commandBegin = serverCommandBegin
    commandEnd = serverCommandEnd
    commandWorkdir = serverCommandWorkdir
    commandUser = serverCommandUser
    commandHost = serverCommandHost
    commandExit = serverCommandExit

makeServerCommand :: UserId -> Command -> ServerCommand
makeServerCommand serverCommandServerUser Command {..} = ServerCommand {..}
  where
    serverCommandText = commandText
    serverCommandBegin = commandBegin
    serverCommandEnd = commandEnd
    serverCommandWorkdir = commandWorkdir
    serverCommandUser = commandUser
    serverCommandHost = commandHost
    serverCommandExit = commandExit
