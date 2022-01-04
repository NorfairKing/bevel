{-# LANGUAGE RecordWildCards #-}

module Bevel.API.Server.Handler.Sync where

import Bevel.API.Server.Handler.Import
import qualified Data.Appendful.Persistent as Appendful

handlePostSync :: AuthCookie -> SyncRequest -> H SyncResponse
handlePostSync AuthCookie {..} SyncRequest {..} = withUser authCookieUsername $ \(Entity uid _) ->
  runDB $ do
    syncResponseCommandSyncResponse <-
      Appendful.serverProcessSyncQuery
        [ServerCommandServerUser ==. uid]
        serverMakeCommand
        (makeServerCommand uid)
        syncRequestCommandSyncRequest
    pure SyncResponse {..}
