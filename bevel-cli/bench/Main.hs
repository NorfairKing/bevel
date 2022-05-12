{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bevel.CLI.Choices
import Bevel.CLI.Commands.ChangeDir (changeDirLoadSource)
import Bevel.CLI.Commands.RepeatCommand (repeatCommandLoadSource)
import Bevel.CLI.Select
import Bevel.Client.Data
import Bevel.Client.Data.Gen ()
import Conduit
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Criterion.Main as Criterion
import qualified Data.Conduit.Combinators as C
import Data.GenValidity
import Data.Text
import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

main :: IO ()
main = do
  let now = UTCTime (fromGregorian 2022 05 12) 12345
  pool <- makeDatabase
  setupDatabase pool
  Criterion.defaultMain
    [ bgroup
        "RepeatCommand"
        [ bench "loadAllChoices" $
            whnfIO
              ( runResourceT
                  ( runSqlPool
                      ( loadAllChoices
                          repeatCommandLoadSource
                          now
                      )
                      pool
                  )
              )
        ],
      bgroup
        "ChangeDir"
        [ bench "loadAllChoices" $
            whnfIO
              ( runResourceT
                  ( runSqlPool
                      ( loadAllChoices
                          (changeDirLoadSource benchHostname benchUsername)
                          now
                      )
                      pool
                  )
              )
        ]
    ]

makeDatabase :: IO ConnectionPool
makeDatabase = do
  pool <- runNoLoggingT $ createSqlitePool "bench-database.sqlite3" 1
  _ <- runSqlPool (runMigrationQuiet clientMigration) pool
  pure pool

setupDatabase :: ConnectionPool -> IO ()
setupDatabase pool =
  flip runSqlPool pool $ do
    let qcgen = mkQCGen 42
    let numberOfCommands = 10000
    let genCommand :: Gen ClientCommand
        genCommand = do
          command <- genValid
          let command' =
                command
                  { clientCommandHost = benchHostname,
                    clientCommandUser = benchUsername
                  }
          pure command'
    let gen = replicateM numberOfCommands genCommand
    let commands = unGen gen qcgen 30
    insertMany_ commands

loadAllChoices :: LoadSource -> UTCTime -> SqlPersistT (ResourceT IO) Choices
loadAllChoices loadSource now = runConduit $ loadChoices loadSource now "" .| C.fold

benchHostname :: Text
benchHostname = "bench-host"

benchUsername :: Text
benchUsername = "bench-user"
