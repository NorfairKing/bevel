{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Bevel.GatherSpec (spec) where

import Bevel.Client.Data
import Bevel.Client.Data.Gen ()
import Control.Concurrent.Async
import Control.Monad.Logger
import Data.Int
import Data.Maybe
import qualified Data.Text as T
import Database.Persist.Sqlite
import Path
import Path.IO
import System.Exit
import System.Process.Typed
import Test.Syd
import Test.Syd.Path
import Test.Syd.Validity

spec :: Spec
spec = tempDirSpec "bevel" $ do
  describe "before" $ do
    it "can gather the start of a command" $ \tdir ->
      forAllValid $ \text -> do
        databaseFile <- resolveFile tdir "history.sqlite"
        runNoLoggingT $
          withSqlitePool (T.pack (fromAbsFile databaseFile)) 1 $ \pool -> do
            _ <- runSqlPool (runMigrationQuiet clientMigration) pool
            pure ()

        let env = [("BEVEL_DATABASE", fromAbsFile databaseFile)]
        let pc = setStdout closed $ setEnv env $ setWorkingDir (fromAbsDir tdir) $ proc "bevel-gather-before" [T.unpack text]
        ec <- runProcess pc
        ec `shouldBe` ExitSuccess

        commands <- runNoLoggingT $
          withSqlitePool (T.pack (fromAbsFile databaseFile)) 1 $ \pool -> do
            runSqlPool (selectList [] [Asc ClientCommandId]) pool

        case commands of
          [Entity cid ClientCommand {..}] -> do
            cid `shouldBe` toSqlKey 1
            let ClientCommand _ _ _ _ _ _ _ _ = undefined
            clientCommandText `shouldBe` text
            clientCommandWorkdir `shouldBe` tdir
            clientCommandExit `shouldBe` Nothing
            clientCommandServerId `shouldBe` Nothing
          _ -> expectationFailure "expected a single command"

    modifyMaxSuccess (`div` 10) $
      it "works concurrently" $ \tdir -> do
        forAllValid $ \text -> do
          databaseFile <- resolveFile tdir "history.sqlite"
          runNoLoggingT $
            withSqlitePool (T.pack (fromAbsFile databaseFile)) 1 $ \pool -> do
              _ <- runSqlPool (runMigrationQuiet clientMigration) pool
              pure ()

          let env = [("BEVEL_DATABASE", fromAbsFile databaseFile)]
          let pc = setStdout closed $ setStderr closed $ setEnv env $ setWorkingDir (fromAbsDir tdir) $ proc "bevel-gather-before" [T.unpack text]
          replicateConcurrently_ 10 $ runProcess pc

          commands <- runNoLoggingT $
            withSqlitePool (T.pack (fromAbsFile databaseFile)) 1 $ \pool -> do
              runSqlPool (selectList [] [Asc ClientCommandId]) pool
          shouldBeValid commands

  describe "after" $ do
    it "can gather the end of a command" $ \tdir ->
      forAllValid $ \text ->
        forAllValid $ \begin ->
          forAllValid $ \workdir ->
            forAllValid $ \exitCode ->
              forAllValid $ \user ->
                forAllValid $ \host -> do
                  databaseFile <- resolveFile tdir "history.sqlite"
                  cid <- runNoLoggingT $
                    withSqlitePool (T.pack (fromAbsFile databaseFile)) 1 $ \pool -> flip runSqlPool pool $ do
                      _ <- runMigrationQuiet clientMigration
                      insert
                        ClientCommand
                          { clientCommandText = text,
                            clientCommandBegin = begin,
                            clientCommandEnd = Nothing,
                            clientCommandWorkdir = workdir,
                            clientCommandUser = user,
                            clientCommandHost = host,
                            clientCommandExit = Nothing,
                            clientCommandServerId = Nothing
                          }

                  let env = [("BEVEL_DATABASE", fromAbsFile databaseFile)]
                  let pc = setStdout closed $ setEnv env $ setWorkingDir (fromAbsDir tdir) $ proc "bevel-gather-after" [show (fromSqlKey cid), show exitCode]
                  ec <- runProcess pc
                  ec `shouldBe` ExitSuccess

                  commands <- runNoLoggingT $
                    withSqlitePool (T.pack (fromAbsFile databaseFile)) 1 $ \pool -> do
                      runSqlPool (selectList [] [Asc ClientCommandId]) pool

                  case commands of
                    [Entity cid' ClientCommand {..}] -> do
                      cid' `shouldBe` cid
                      let ClientCommand _ _ _ _ _ _ _ _ = undefined
                      clientCommandText `shouldBe` text
                      clientCommandBegin `shouldBe` begin
                      clientCommandEnd `shouldSatisfy` isJust
                      clientCommandWorkdir `shouldBe` workdir
                      clientCommandUser `shouldBe` user
                      clientCommandHost `shouldBe` host
                      clientCommandExit `shouldBe` Just exitCode
                      clientCommandServerId `shouldBe` Nothing
                    _ -> expectationFailure "expected a single command"

    modifyMaxSuccess (`div` 10) $
      it "works concurrently" $ \tdir -> do
        forAllValid $ \text ->
          forAllValid $ \begin ->
            forAllValid $ \workdir ->
              forAllValid $ \exitCode ->
                forAllValid $ \user ->
                  forAllValid $ \host -> do
                    databaseFile <- resolveFile tdir "history.sqlite"
                    cid <- runNoLoggingT $
                      withSqlitePool (T.pack (fromAbsFile databaseFile)) 1 $ \pool -> flip runSqlPool pool $ do
                        _ <- runMigrationQuiet clientMigration
                        insert
                          ClientCommand
                            { clientCommandText = text,
                              clientCommandBegin = begin,
                              clientCommandEnd = Nothing,
                              clientCommandWorkdir = workdir,
                              clientCommandUser = user,
                              clientCommandHost = host,
                              clientCommandExit = Nothing,
                              clientCommandServerId = Nothing
                            }

                    let env = [("BEVEL_DATABASE", fromAbsFile databaseFile)]
                    let pc = setStdout closed $ setStderr closed $ setEnv env $ setWorkingDir (fromAbsDir tdir) $ proc "bevel-gather-after" [show (fromSqlKey cid), show (exitCode :: Int8)]
                    replicateConcurrently_ 10 $ runProcess pc

                    commands <- runNoLoggingT $
                      withSqlitePool (T.pack (fromAbsFile databaseFile)) 1 $ \pool -> do
                        runSqlPool (selectList [] [Asc ClientCommandId]) pool

                    case commands of
                      [Entity cid' ClientCommand {..}] -> do
                        cid' `shouldBe` cid
                        let ClientCommand _ _ _ _ _ _ _ _ = undefined
                        clientCommandText `shouldBe` text
                        clientCommandBegin `shouldBe` begin
                        clientCommandEnd `shouldSatisfy` isJust
                        clientCommandWorkdir `shouldBe` workdir
                        clientCommandUser `shouldBe` user
                        clientCommandHost `shouldBe` host
                        clientCommandExit `shouldBe` Just exitCode
                        clientCommandServerId `shouldBe` Nothing
                      _ -> expectationFailure "expected a single command"
