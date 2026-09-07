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
import Bevel.Data.Gen ()
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Appendful as Appendful
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sqlite
import Path
import Path.IO
import Test.QuickCheck (choose, forAll, vectorOf)
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = serverSpec $
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
