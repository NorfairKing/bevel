{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bevel.API.Server where

import Bevel.API as API
import Bevel.API.Server.Env
import Bevel.API.Server.Handler
import Bevel.API.Server.OptParse
import Bevel.API.Server.SigningKey
import Control.Monad
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist.Sql
import Database.Persist.Sqlite
import Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Path
import Servant.Auth.Server
import Servant.Server.Generic
import System.Exit
import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import System.Metrics.Prometheus.Wai.Middleware
import UnliftIO

bevelAPIServer :: IO ()
bevelAPIServer = do
  Settings {..} <- getSettings
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= settingLogLevel) $
      withSqlitePool (T.pack (fromAbsFile settingDbFile)) 1 $ \pool -> do
        runSqlPool (completeServerMigrations False) pool
        jwk <- liftIO $ loadSigningKey settingSigningKeyFile
        let serverEnv =
              Env
                { envConnectionPool = pool,
                  envHashDifficulty = 10,
                  envCookieSettings = defaultCookieSettings,
                  envJWTSettings = defaultJWTSettings jwk
                }
        logFunc <- askLoggerIO
        loggingMiddleware <-
          liftIO $
            mkRequestLogger
              defaultRequestLoggerSettings
                { destination = Callback $ \str ->
                    logFunc defaultLoc "warp" LevelInfo str,
                  outputFormat = Apache FromSocket
                }
        registry <- liftIO Registry.new
        waiMetrics <- liftIO $ registerWaiMetrics mempty registry
        let middlewares =
              metricsEndpointMiddleware registry
                . instrumentWaiMiddleware waiMetrics
                . loggingMiddleware
        let completedApp = middlewares $ bevelAPIServerApp logFunc serverEnv
        let sets = Warp.setPort settingPort Warp.defaultSettings
        liftIO $ Warp.runSettings sets completedApp

completeServerMigrations :: (MonadUnliftIO m, MonadLogger m) => Bool -> SqlPersistT m ()
completeServerMigrations quiet = do
  logInfoN "Running automatic migrations"
  (if quiet then void . runMigrationQuiet else runMigration) automaticServerMigrations
    `catch` ( \case
                PersistError t -> liftIO $ die $ T.unpack t
                e -> throwIO e
            )
  logInfoN "Autmatic migrations done, starting application-specific migrations."
  setUpIndices
  logInfoN "Migrations done."

setUpIndices :: (MonadIO m) => SqlPersistT m ()
setUpIndices = do
  rawExecute "CREATE UNIQUE INDEX IF NOT EXISTS command_server_id ON command (id)" []
  rawExecute "CREATE INDEX IF NOT EXISTS command_server_user ON command (server_user)" []

{-# ANN bevelAPIServerApp ("NOCOVER" :: String) #-}
bevelAPIServerApp ::
  (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) ->
  Env ->
  Wai.Application
bevelAPIServerApp logFunc env =
  genericServeTWithContext
    (flip runLoggingT logFunc . flip runReaderT env)
    bevelHandlers
    (bevelContext env)

bevelContext :: Env -> Context '[CookieSettings, JWTSettings]
bevelContext Env {..} = envCookieSettings :. envJWTSettings :. EmptyContext

bevelHandlers :: BevelRoutes (AsServerT H)
bevelHandlers =
  BevelRoutes
    { postRegister = handlePostRegister,
      postLogin = handlePostLogin,
      postSync = protected handlePostSync
    }

protected :: (ThrowAll m) => (authCookie -> m) -> AuthResult authCookie -> m
protected func (Authenticated authCookie) = func authCookie
protected _ _ = throwAll err401
