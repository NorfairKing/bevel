{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bevel.CLI.Commands.Sync where

import Bevel.CLI.Commands.Import
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.Appendful.Persistent as Appendful
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE

sync :: C ()
sync = withClient $ \cenv -> withLogin cenv $ \token -> do
  req <- runDB $ do
    syncRequestCommandSyncRequest <-
      Appendful.clientMakeSyncRequestQuery
        clientMakeCommand
        ClientCommandServerId
    pure SyncRequest {..}
  logDebugN $ "Request: " <> TE.decodeUtf8 (LB.toStrict $ JSON.encodePretty req)
  resp@SyncResponse {..} <- runClientOrDie cenv $ postSync bevelClient token req
  logDebugN $ "Response: " <> TE.decodeUtf8 (LB.toStrict $ JSON.encodePretty resp)
  runDB $ do
    Appendful.clientMergeSyncResponseQuery
      makeSyncedClientCommand
      ClientCommandServerId
      syncResponseCommandSyncResponse
