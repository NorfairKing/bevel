{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bevel.CLI
  ( bevelCLI,
    completeCliMigrations,
  )
where

import Bevel.CLI.Commands as Commands
import qualified Data.Text as T
import Database.Persist.Sqlite
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Path
import Path.IO
import System.Exit
import System.FileLock
import UnliftIO

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
                _ <- runSqlPool (completeCliMigrations False) pool
                let env =
                      Env
                        { envClientEnv = mCenv,
                          envUsername = settingUsername,
                          envPassword = settingPassword,
                          envMaxOptions = settingMaxOptions,
                          envConnectionPool = pool
                        }
                runReaderT func env
  case dispatch of
    DispatchRegister -> runC Shared Commands.register
    DispatchLogin -> runC Shared Commands.login
    DispatchSync -> runC Exclusive Commands.sync
    DispatchLast -> runC Shared Commands.lastDir

completeCliMigrations :: (MonadUnliftIO m, MonadLogger m) => Bool -> SqlPersistT m ()
completeCliMigrations quiet = do
  logInfoN "Running automatic migrations"
  (if quiet then void . runMigrationQuiet else runMigration) automaticClientMigrations
    `catch` ( \case
                PersistError t -> liftIO $ die $ T.unpack t
                e -> throwIO e
            )
  logInfoN "Autmatic migrations done, starting application-specific migrations."
  setUpIndices
  logInfoN "Migrations done."

setUpIndices :: MonadIO m => SqlPersistT m ()
setUpIndices = do
  rawExecute "CREATE INDEX IF NOT EXISTS command_text ON command (text)" []
  rawExecute "CREATE INDEX IF NOT EXISTS command_begin ON command (begin)" []
  rawExecute "CREATE INDEX IF NOT EXISTS command_begin_end ON command (begin,end)" []
  rawExecute "CREATE INDEX IF NOT EXISTS command_workdir ON command (workdir)" []
  rawExecute "CREATE INDEX IF NOT EXISTS command_user_host ON command (user,host)" []
  rawExecute "CREATE INDEX IF NOT EXISTS command_user_host_begin ON command (user,host,begin)" []
