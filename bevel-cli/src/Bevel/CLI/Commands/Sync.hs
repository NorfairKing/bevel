{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bevel.CLI.Commands.Sync where

import Bevel.API.Server.Data (ServerCommandId)
import Bevel.CLI.Commands.Import
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.Appendful.Persistent as Appendful
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as M
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql (SqlPersistT)
import System.Exit

sync :: C ()
sync = withClient $ \cenv -> withLogin cenv $ \token -> do
  download cenv token
  appendfulSync cenv token

-- | Fetch the commands the server already has, in batches.
--
-- The appendful sync cannot do this: its server-side read is unbounded, so a
-- client catching up on a large history asks the server for a response it
-- cannot build in time.
download :: ClientEnv -> Token -> C ()
download cenv token = go
  where
    go :: C ()
    go = do
      downloadRequestMaximumSynced <- runDB clientMaximumSyncedCommandId
      resp <-
        runClientOrDie cenv $
          postDownload bevelClient token DownloadRequest {..}
      let commands = downloadResponseCommands resp
      case fst <$> M.lookupMax commands of
        -- An empty batch means we have everything the server has, so the
        -- client never needs to know the server's batch size.
        Nothing -> pure ()
        Just greatestInBatch ->
          if Just greatestInBatch > downloadRequestMaximumSynced
            then do
              runDB $
                forM_ (M.toList commands) $ \(sid, command) ->
                  insert_ $ makeSyncedClientCommand sid command
              go
            else
              liftIO $
                die $
                  unwords
                    [ "The server sent a batch of commands that would not advance the download cursor beyond",
                      show downloadRequestMaximumSynced,
                      "so downloading cannot make progress."
                    ]

-- | The greatest server id the client has.
--
-- This is a valid download cursor: every server id in the client database
-- either came from a download batch, and batches are taken in ascending server
-- id order, or was assigned by the server to a command this client uploaded,
-- which is greater than every id the server had at that point. Either way the
-- client has every command up to this one.
clientMaximumSyncedCommandId :: SqlPersistT IO (Maybe ServerCommandId)
clientMaximumSyncedCommandId = do
  mEntity <-
    selectFirst
      [ClientCommandServerId !=. Nothing]
      [Desc ClientCommandServerId]
  pure $ clientCommandServerId . entityVal =<< mEntity

appendfulSync :: ClientEnv -> Token -> C ()
appendfulSync cenv token = do
  req <- runDB $ do
    syncRequestCommandSyncRequest <-
      Appendful.clientMakeSyncRequestQuery
        clientMakeCommand
        ClientCommandServerId
    pure SyncRequest {..}
  logDebugN $ "Request: " <> TE.decodeUtf8 (LB.toStrict $ JSON.encodePretty req)
  resp@SyncResponse {..} <- runClientOrDie cenv $ postSync bevelClient token req
  logDebugN $ "Response: " <> TE.decodeUtf8 (LB.toStrict $ JSON.encodePretty resp)
  runDB $
    Appendful.clientMergeSyncResponseQuery
      makeSyncedClientCommand
      ClientCommandServerId
      syncResponseCommandSyncResponse
