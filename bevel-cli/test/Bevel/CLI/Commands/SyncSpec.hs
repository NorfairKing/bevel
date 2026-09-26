module Bevel.CLI.Commands.SyncSpec (spec) where

import Bevel.API
import Bevel.API.Data
import Bevel.API.Data.Gen ()
import Bevel.API.Server.TestUtils
import Bevel.CLI
import Bevel.CLI.Commands.Sync
import Bevel.CLI.Env
import Bevel.Client
import Bevel.Client.Data
import Bevel.Data
import Bevel.Data.Gen ()
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Appendful as Appendful
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Word (Word64)
import Database.Persist
import Database.Persist.Sqlite
import Path
import Path.IO
import Test.QuickCheck (choose, forAll, vectorOf)
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  clientMakeCommandSyncRequestSpec
  serverSpec $
    describe "download" $
      it "downloads a history that spans more than one batch onto a fresh client" $
        \cenv ->
          let genCommands = do
                n <- choose (testDownloadBatchSize + 1, 3 * testDownloadBatchSize)
                vectorOf n genValid
           in forAll genCommands $ \commands -> forAllValid $ \rf ->
                withSystemTempDir "bevel-download" $ \tdir -> withNewUser cenv rf $ \token -> do
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
                  dbFile <- resolveFile tdir "bevel-client.sqlite3"
                  downloaded <-
                    runNoLoggingT $
                      withSqlitePool (T.pack (fromAbsFile dbFile)) 1 $ \pool -> do
                        void $ runSqlPool (completeCliMigrations True) pool
                        let env =
                              Env
                                { envClientEnv = Just cenv,
                                  envUsername = Nothing,
                                  envPassword = Nothing,
                                  envMaxOptions = 15,
                                  envConnectionPool = pool
                                }
                        liftIO $
                          flip runLoggingT (\_ _ _ _ -> pure ()) $
                            runReaderT (download cenv token) env
                        runSqlPool (selectList [] [Asc ClientCommandId]) pool
                  map (clientMakeCommand . entityVal) downloaded `shouldBe` commands

clientMakeCommandSyncRequestSpec :: Spec
clientMakeCommandSyncRequestSpec =
  describe "clientMakeCommandSyncRequest" $ do
    it "offers a command that has finished" $
      forAllValid $ \command ->
        offeredCommands (command {commandEnd = Just (commandBegin command)}) now
          `shouldReturn` 1

    it "holds back a command that is still running" $
      forAllValid $ \command ->
        offeredCommands (command {commandBegin = now, commandEnd = Nothing}) now
          `shouldReturn` 0

    it "offers a command that is still running but old enough to have been abandoned" $
      forAllValid $ \command ->
        offeredCommands
          (command {commandBegin = now - abandonedAfter - 1, commandEnd = Nothing})
          now
          `shouldReturn` 1
  where
    -- Far enough past 1970 that taking a day off it is not remarkable.
    now :: Word64
    now = 2 * abandonedAfter

    offeredCommands :: Command -> Word64 -> IO Int
    offeredCommands command at =
      runNoLoggingT $
        withSqlitePool (T.pack ":memory:") 1 $ \pool -> do
          void $ runSqlPool (completeCliMigrations True) pool
          runSqlPool
            ( do
                insert_ (makeUnsyncedClientCommand command)
                M.size . Appendful.syncRequestAdded
                  <$> clientMakeCommandSyncRequest at
            )
            pool
