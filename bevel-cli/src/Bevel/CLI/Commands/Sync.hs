{-# LANGUAGE RecordWildCards #-}

module Bevel.CLI.Commands.Sync where

import Bevel.CLI.Commands.Import
import qualified Data.Appendful.Persistent as Appendful

sync :: C ()
sync = withClient $ \cenv -> withLogin cenv $ \token -> do
  req <- runDB $ do
    syncRequestCommandSyncRequest <-
      Appendful.clientMakeSyncRequestQuery
        clientMakeCommand
        ClientCommandServerId
    pure SyncRequest {..}
  SyncResponse {..} <- runClientOrDie cenv $ postSync bevelClient token req
  runDB $ do
    Appendful.clientMergeSyncResponseQuery
      makeSyncedClientCommand
      ClientCommandServerId
      syncResponseCommandSyncResponse
