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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bevel.API.Server.Data.DB where

import Bevel.API.Server.Data.Username
import Bevel.Data
import Control.Arrow (left)
import Data.Int
import Data.Password.Bcrypt
import Data.Password.Instances ()
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Persist ()
import Data.Word
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics (Generic)
import Path

share
  [mkPersist sqlSettings, mkMigrate "serverMigration"]
  [persistLowerCase|

User
  name Username
  password (PasswordHash Bcrypt)

  UniqueUsername name

  deriving Show Eq Ord Generic


ServerCommand sql=command
  serverUser UserId

  text Text
  begin Word64
  end Word64 Maybe default=NULL
  workdir (Path Abs Dir)
  user Text
  host Text
  exit Int8 Maybe default=NULL

  deriving Show
  deriving Eq
  deriving Ord
  deriving Generic

|]

instance Validity (Salt a) where
  validate = trivialValidation

instance Validity Password where
  validate = trivialValidation

instance Validity (PasswordHash a) where
  validate = trivialValidation

instance Validity User

instance Validity ServerCommand

instance PersistField (Path Abs Dir) where
  toPersistValue = toPersistValue . fromAbsDir
  fromPersistValue pv = do
    s <- fromPersistValue pv
    left (T.pack . show) $ parseAbsDir s

instance PersistFieldSql (Path Abs Dir) where
  sqlType Proxy = sqlType (Proxy :: Proxy String)

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
