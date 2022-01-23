{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bevel.CLI.Env where

import Bevel.API
import Bevel.API.Data
import Bevel.API.Server.Data
import Bevel.Client
import Control.Monad.Logger
import Control.Monad.Reader
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql
import System.Exit
import Web.Cookie

type C a = ReaderT Env (LoggingT IO) a

data Env = Env
  { envClientEnv :: !(Maybe ClientEnv),
    envUsername :: !(Maybe Username),
    envPassword :: !(Maybe Text),
    envConnectionPool :: !ConnectionPool
  }

withClient :: (ClientEnv -> C a) -> C a
withClient func = do
  mCenv <- asks envClientEnv
  case mCenv of
    Nothing -> liftIO $ die "No server configured."
    Just cenv -> func cenv

runDB :: SqlPersistT IO a -> C a
runDB func = do
  pool <- asks envConnectionPool
  liftIO $ runSqlPool func pool

runClientOrDie :: ClientEnv -> ClientM a -> C a
runClientOrDie cenv func = do
  errOrRes <- runClient cenv func
  case errOrRes of
    Left err -> liftIO $ die $ show err
    Right res -> pure res

runClient :: ClientEnv -> ClientM a -> C (Either ClientError a)
runClient cenv func = liftIO $ runClientM func cenv

getEnvUsername :: C Username
getEnvUsername = do
  mUsername <- asks envUsername
  case mUsername of
    Nothing -> liftIO $ die "No username configured." -- TODO prompt
    Just un -> pure un

getEnvPassword :: C Text
getEnvPassword = do
  mPassword <- asks envPassword
  case mPassword of
    Nothing -> liftIO $ die "No password configured." -- TODO prompt
    Just pw -> pure pw

withLogin :: ClientEnv -> (Token -> C a) -> C a
withLogin cenv func = do
  loginFormUsername <- getEnvUsername
  loginFormPassword <- getEnvPassword
  let lf = LoginForm {..}
  Headers NoContent (HCons sessionHeader HNil) <- runClientOrDie cenv $ postLogin bevelClient lf
  case sessionHeader of
    MissingHeader -> liftIO $ die "The server responded but the response was missing the right session header."
    UndecodableHeader _ -> liftIO $ die "The server responded but the response had an undecodable session header."
    Header setCookieText -> do
      let cookies = parseSetCookie . TE.encodeUtf8 <$> T.lines setCookieText
          jwtCookie = find ((== "JWT-Cookie") . setCookieName) cookies
      case jwtCookie of
        Nothing -> liftIO $ die "No JWT-Cookie was found in the Set-Cookie session header."
        Just setCookie -> func $ Token $ setCookieValue setCookie
