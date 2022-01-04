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
import Data.Password.Bcrypt
import Data.Password.Instances ()
import Data.Validity
import Data.Validity.Persist ()
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "serverMigration"]
  [persistLowerCase|

User
  name Username
  password (PasswordHash Bcrypt)

  UniqueUsername name

  deriving Show Eq Ord Generic


ServerAppendfulThing sql=appendful_thing
  user UserId
  number Int

  deriving Show Eq Ord Generic

|]

instance Validity (Salt a) where
  validate = trivialValidation

instance Validity Password where
  validate = trivialValidation

instance Validity (PasswordHash a) where
  validate = trivialValidation

instance Validity User

instance Validity ServerAppendfulThing

serverAppendfulMakeThing :: ServerAppendfulThing -> Thing
serverAppendfulMakeThing ServerAppendfulThing {..} = Thing {..}
  where
    thingNumber = serverAppendfulThingNumber

makeServerAppendfulThing :: UserId -> Thing -> ServerAppendfulThing
makeServerAppendfulThing serverAppendfulThingUser Thing {..} = ServerAppendfulThing {..}
  where
    serverAppendfulThingNumber = thingNumber
