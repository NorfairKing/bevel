{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bevel.CLI
  ( bevelCLI,
  )
where

import Bevel.CLI.Commands as Commands
import qualified Data.Text as T
import Database.Persist.Sqlite
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Path
import Path.IO
import System.FileLock

bevelCLI :: IO ()
bevelCLI = do
  Instructions dispatch Settings {..} <- getInstructions
  mCenv <- forM settingBaseUrl $ \burl -> do
    man <- HTTP.newManager HTTP.tlsManagerSettings
    pure $ mkClientEnv man burl
  ensureDir $ parent settingDbFile
  let runC lockType func = do
        -- Block until locking succeeds
        withFileLock (fromAbsFile settingDbFile ++ ".lock") lockType $ \_ ->
          runStderrLoggingT $
            filterLogger (\_ ll -> ll >= settingLogLevel) $
              withSqlitePool (T.pack (fromAbsFile settingDbFile)) 1 $ \pool -> do
                _ <- runSqlPool (runMigrationQuiet clientMigration) pool
                let env =
                      Env
                        { envClientEnv = mCenv,
                          envUsername = settingUsername,
                          envPassword = settingPassword,
                          envConnectionPool = pool
                        }
                runReaderT func env
  case dispatch of
    DispatchRegister -> runC Shared Commands.register
    DispatchLogin -> runC Shared Commands.login
    DispatchSync -> runC Exclusive Commands.sync
    DispatchChangeDir -> runC Shared Commands.changeDir
    DispatchRepeat -> runC Shared Commands.repeatCommand
    DispatchRepeatLocal -> runC Shared Commands.repeatLocalCommand
    DispatchLast -> runC Shared Commands.lastDir
