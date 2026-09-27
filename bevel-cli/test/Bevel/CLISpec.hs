{-# LANGUAGE OverloadedStrings #-}

module Bevel.CLISpec (spec) where

import Bevel.API.Data
import Bevel.API.Server.Data
import Bevel.API.Server.TestUtils
import Bevel.CLI
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist.Sqlite
import Path
import Path.IO
import Servant.Client
import System.Environment
import System.Exit
import System.Process.Typed (closed, proc, runProcess, setStdout, setWorkingDir)
import qualified System.Process.Typed as Process (setEnv)
import Test.Syd
import Test.Syd.Validity

-- Sequential because these tests set process-global arguments and environment.
spec :: Spec
spec = sequential $ do
  describe "migrate" $
    it "sets up a database that bevel-gather can write to" $
      withSystemTempDir "bevel-migrate" $ \tdir -> do
        dataDir <- resolveDir tdir "data"
        dbFile <- resolveFile dataDir "history.sqlite3"
        withArgs ["migrate", "--database", fromAbsFile dbFile] bevelCLI
        let pc =
              setStdout closed $
                Process.setEnv [("BEVEL_DATABASE", fromAbsFile dbFile)] $
                  setWorkingDir (fromAbsDir tdir) $
                    proc "bevel-gather" ["echo hi"]
        ec <- runProcess pc
        ec `shouldBe` ExitSuccess

  describe "setUpIndices" $ do
    it "leaves exactly the indices that something reads" $
      withSystemTempDir "bevel-indices" $ \tdir -> do
        dbFile <- resolveFile tdir "history.sqlite3"
        indices <- runNoLoggingT $
          withSqlitePool (T.pack (fromAbsFile dbFile)) 1 $ \pool ->
            flip runSqlPool pool $ do
              completeCliMigrations True
              map unSingle
                <$> rawSql
                  "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = 'command' AND name NOT LIKE 'sqlite_%' ORDER BY name"
                  []
        indices
          `shouldBe` [ "command_begin" :: T.Text,
                       "command_server_id",
                       "command_user_host_begin",
                       "command_workdir_begin"
                     ]

    it "drops the two it used to make, on a history that already has them" $
      withSystemTempDir "bevel-indices" $ \tdir -> do
        dbFile <- resolveFile tdir "history.sqlite3"
        indices <- runNoLoggingT $
          withSqlitePool (T.pack (fromAbsFile dbFile)) 1 $ \pool ->
            flip runSqlPool pool $ do
              completeCliMigrations True
              rawExecute "CREATE INDEX command_text ON command (text)" []
              rawExecute "CREATE INDEX command_exit ON command (exit)" []
              completeCliMigrations True
              map unSingle
                <$> rawSql
                  "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = 'command' AND name NOT LIKE 'sqlite_%' ORDER BY name"
                  []
        indices
          `shouldBe` [ "command_begin" :: T.Text,
                       "command_server_id",
                       "command_user_host_begin",
                       "command_workdir_begin"
                     ]

  serverSpec $
    describe "Bevel CLI" $
      it "'just works'" $
        \cenv -> forAllValid $ \rf -> withSystemTempDir "bevel-cli" $ \tdir -> do
          dbFile <- resolveFile tdir "bevel-client.sqlite3"
          let testBevel args = do
                setEnv "BEVEL_SERVER_URL" $ showBaseUrl $ baseUrl cenv
                setEnv "BEVEL_USERNAME" $ T.unpack $ usernameText $ registrationFormUsername rf
                setEnv "BEVEL_PASSWORD" $ T.unpack $ registrationFormPassword rf
                setEnv "BEVEL_DATABASE" $ fromAbsFile dbFile
                withArgs args bevelCLI
          testBevel ["register"]
          testBevel ["login"]
          testBevel ["sync"]
