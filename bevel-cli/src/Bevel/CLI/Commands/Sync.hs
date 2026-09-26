{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bevel.CLI.Commands.Sync where

import Bevel.API.Server.Data (ServerCommandId)
import Bevel.CLI.Commands.Import
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.Appendful as Appendful
import qualified Data.Appendful.Persistent as Appendful
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
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
clientMaximumSyncedCommandId :: (MonadIO m) => SqlPersistT m (Maybe ServerCommandId)
clientMaximumSyncedCommandId = do
  mEntity <-
    selectFirst
      [ClientCommandServerId !=. Nothing]
      [Desc ClientCommandServerId]
  pure $ clientCommandServerId . entityVal =<< mEntity

-- | The current time, in the nanoseconds since 1970 that a command begin is in.
nowInNanoseconds :: IO Word64
nowInNanoseconds = floor . (* 1_000_000_000) . toRational <$> getPOSIXTime

-- | How long a command is given to finish before it is taken to never will.
--
-- A command still unfinished after this has almost certainly lost its shell,
-- so waiting any longer would only keep it off the server.  Waiting less would
-- upload commands that are merely slow.
abandonedAfter :: Word64
abandonedAfter = 24 * 60 * 60 * 1000 * 1000 * 1000

-- | The commands to offer the server, and how far this client has synced.
--
-- Appendful requires the items it syncs to be immutable, but bevel-gather
-- writes a command when it starts and fills in its end and exit code when it
-- finishes.  Uploading one that is still running freezes it on the server
-- without either, where it reads as an interrupted command for good, and no
-- later sync can correct it.  So a command is offered only once it has
-- finished, or once it is old enough that it never will.
--
-- This is `Appendful.clientMakeSyncRequestQuery` with that condition added,
-- which it has no room for.
clientMakeCommandSyncRequest ::
  (MonadIO m) =>
  Word64 ->
  SqlPersistT m (Appendful.SyncRequest ClientCommandId ServerCommandId Command)
clientMakeCommandSyncRequest now = do
  -- Saturating, because taking a day off a Word64 that is smaller than a day
  -- would wrap around and call every command abandoned.
  let abandonedBefore = if now > abandonedAfter then now - abandonedAfter else 0
  syncRequestAdded <-
    M.fromList . map (\(Entity cid ct) -> (cid, clientMakeCommand ct))
      <$> selectList
        ( (ClientCommandServerId ==. Nothing)
            : ( [ClientCommandEnd !=. Nothing]
                  ||. [ClientCommandBegin <. abandonedBefore]
              )
        )
        []
  syncRequestMaximumSynced <- clientMaximumSyncedCommandId
  pure Appendful.SyncRequest {..}

appendfulSync :: ClientEnv -> Token -> C ()
appendfulSync cenv token = do
  now <- liftIO nowInNanoseconds
  req <- runDB $ do
    syncRequestCommandSyncRequest <- clientMakeCommandSyncRequest now
    pure SyncRequest {..}
  logDebugN $ T.unwords ["Request:", TE.decodeUtf8 (LB.toStrict (JSON.encodePretty req))]
  resp@SyncResponse {..} <- runClientOrDie cenv $ postSync bevelClient token req
  logDebugN $ T.unwords ["Response:", TE.decodeUtf8 (LB.toStrict (JSON.encodePretty resp))]
  runDB $
    Appendful.clientMergeSyncResponseQuery
      makeSyncedClientCommand
      ClientCommandServerId
      syncResponseCommandSyncResponse
