{-# LANGUAGE RecordWildCards #-}

module Bevel.API.Server.Handler.Download where

import Bevel.API.Server.Handler.Import
import qualified Data.Map.Strict as M

handlePostDownload :: AuthCookie -> DownloadRequest -> H DownloadResponse
handlePostDownload AuthCookie {..} DownloadRequest {..} = withUser authCookieUsername $ \(Entity uid _) -> do
  batchSize <- asks envDownloadBatchSize
  runDB $ do
    -- Ascending, so that the greatest server id in the batch is a cursor the
    -- client can come back with: it has everything up to that id.
    let cursorFilter = case downloadRequestMaximumSynced of
          Nothing -> []
          Just sid -> [ServerCommandId >. sid]
    entities <-
      selectList
        ((ServerCommandServerUser ==. uid) : cursorFilter)
        [ Asc ServerCommandId,
          LimitTo batchSize
        ]
    pure
      DownloadResponse
        { downloadResponseCommands =
            M.fromList $
              map (\(Entity sid sc) -> (sid, serverMakeCommand sc)) entities
        }
