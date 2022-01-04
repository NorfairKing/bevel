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
{-# LANGUAGE UndecidableInstances #-}

module Bevel.Client.Data.DB where

import Bevel.API.Server.Data
import Bevel.Data
import Database.Persist.Sqlite
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "clientMigration"]
  [persistLowerCase|

ClientAppendfulThing sql=appendful_thing
    number Int
    serverId ServerAppendfulThingId Maybe
    deriving Show Eq

|]

clientAppendfulMakeThing :: ClientAppendfulThing -> Thing
clientAppendfulMakeThing ClientAppendfulThing {..} = Thing {..}
  where
    thingNumber = clientAppendfulThingNumber

makeSyncedClientAppendfulThing :: ServerAppendfulThingId -> Thing -> ClientAppendfulThing
makeSyncedClientAppendfulThing sid = makeClientAppendfulThing (Just sid)

makeUnsyncedClientAppendfulThing :: Thing -> ClientAppendfulThing
makeUnsyncedClientAppendfulThing = makeClientAppendfulThing Nothing

makeClientAppendfulThing :: Maybe ServerAppendfulThingId -> Thing -> ClientAppendfulThing
makeClientAppendfulThing clientAppendfulThingServerId Thing {..} = ClientAppendfulThing {..}
  where
    clientAppendfulThingNumber = thingNumber
