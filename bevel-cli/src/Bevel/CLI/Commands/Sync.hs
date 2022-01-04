{-# LANGUAGE RecordWildCards #-}

module Bevel.CLI.Commands.Sync where

import Bevel.CLI.Commands.Import
import qualified Data.Appendful.Persistent as Appendful

sync :: C ()
sync = withClient $ \cenv -> withLogin cenv $ \token -> do
  req <- runDB $ do
    syncRequestAppendfulThingSyncRequest <-
      Appendful.clientMakeSyncRequestQuery
        clientAppendfulMakeThing
        ClientAppendfulThingServerId
    pure SyncRequest {..}
  SyncResponse {..} <- runClientOrDie cenv $ postSync bevelClient token req
  runDB $ do
    Appendful.clientMergeSyncResponseQuery
      makeSyncedClientAppendfulThing
      ClientAppendfulThingServerId
      syncResponseAppendfulThingSyncResponse
