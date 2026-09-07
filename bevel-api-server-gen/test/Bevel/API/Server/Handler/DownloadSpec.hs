{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bevel.API.Server.Handler.DownloadSpec (spec) where

import Bevel.API
import Bevel.API.Data
import Bevel.API.Data.Gen ()
import Bevel.API.Server.Data
import Bevel.API.Server.TestUtils
import Bevel.Client
import Bevel.Data
import Bevel.Data.Gen ()
import qualified Data.Appendful as Appendful
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Database.Persist.Sql
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = serverSpec $
  describe "PostDownload" $ do
    it "does not crash" $
      \cenv -> forAllValid $ \req -> withAnyNewUser cenv $ \token -> do
        _ <- testClientOrErr cenv $ postDownload bevelClient token req
        pure ()

    it "sends no more than the batch size of commands at a time, and sends every command exactly once" $
      \cenv -> withAnyNewUser cenv $ \token -> do
        let commands = map (\ix -> Command {commandText = "command", commandBegin = ix, commandEnd = Nothing, commandWorkdir = "/", commandUser = "user", commandHost = "host", commandExit = Nothing}) [1 .. fromIntegral (3 * testDownloadBatchSize + 1)]
        _ <-
          testClientOrErr cenv $
            postSync bevelClient token $
              SyncRequest
                { syncRequestCommandSyncRequest =
                    Appendful.SyncRequest
                      { Appendful.syncRequestAdded = M.fromList $ zip (map toSqlKey [1 ..]) commands,
                        Appendful.syncRequestMaximumSynced = Nothing
                      }
                }
        let drain :: Maybe ServerCommandId -> Map ServerCommandId Command -> IO (Map ServerCommandId Command)
            drain downloadRequestMaximumSynced acc = do
              DownloadResponse {..} <-
                testClientOrErr cenv $
                  postDownload bevelClient token DownloadRequest {..}
              M.size downloadResponseCommands `shouldSatisfy` (<= testDownloadBatchSize)
              case fst <$> M.lookupMax downloadResponseCommands of
                Nothing -> pure acc
                Just greatestInBatch -> do
                  greatestInBatch `shouldSatisfy` \sid -> Just sid > downloadRequestMaximumSynced
                  drain (Just greatestInBatch) (M.union acc downloadResponseCommands)
        downloaded <- drain Nothing M.empty
        M.elems downloaded `shouldBe` commands

    it "leaves nothing for the appendful sync to download afterwards" $
      \cenv -> withAnyNewUser cenv $ \token -> do
        let commands = map (\ix -> Command {commandText = "command", commandBegin = ix, commandEnd = Nothing, commandWorkdir = "/", commandUser = "user", commandHost = "host", commandExit = Nothing}) [1 .. fromIntegral (3 * testDownloadBatchSize + 1)]
        _ <-
          testClientOrErr cenv $
            postSync bevelClient token $
              SyncRequest
                { syncRequestCommandSyncRequest =
                    Appendful.SyncRequest
                      { Appendful.syncRequestAdded = M.fromList $ zip (map toSqlKey [1 ..]) commands,
                        Appendful.syncRequestMaximumSynced = Nothing
                      }
                }
        let drain :: Maybe ServerCommandId -> IO (Maybe ServerCommandId)
            drain downloadRequestMaximumSynced = do
              DownloadResponse {..} <-
                testClientOrErr cenv $
                  postDownload bevelClient token DownloadRequest {..}
              case fst <$> M.lookupMax downloadResponseCommands of
                Nothing -> pure downloadRequestMaximumSynced
                Just greatestInBatch -> drain (Just greatestInBatch)
        cursor <- drain Nothing
        SyncResponse {..} <-
          testClientOrErr cenv $
            postSync bevelClient token $
              SyncRequest
                { syncRequestCommandSyncRequest =
                    Appendful.SyncRequest
                      { Appendful.syncRequestAdded = M.empty,
                        Appendful.syncRequestMaximumSynced = cursor
                      }
                }
        Appendful.syncResponseServerAdded syncResponseCommandSyncResponse `shouldBe` M.empty
