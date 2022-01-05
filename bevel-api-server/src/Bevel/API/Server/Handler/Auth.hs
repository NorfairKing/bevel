{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Bevel.API.Server.Handler.Auth where

import Bevel.API.Server.Handler.Import
import Data.Password.Bcrypt
import qualified Data.Text.Encoding as TE
import Servant.Auth.Server (makeSessionCookieBS)

handlePostRegister :: RegistrationForm -> H NoContent
handlePostRegister RegistrationForm {..} = do
  mUser <- runDB $ getBy (UniqueUsername registrationFormUsername)
  case mUser of
    Just _ -> throwError err409
    Nothing -> do
      hashDifficulty <- asks envHashDifficulty
      pass <- hashPasswordWithParams hashDifficulty $ mkPassword registrationFormPassword
      runDB $ insert_ $ User {userName = registrationFormUsername, userPassword = pass}
      pure NoContent

handlePostLogin :: LoginForm -> H (Headers '[Header "Set-Cookie" Text] NoContent)
handlePostLogin LoginForm {..} = do
  mUser <- runDB $ getBy (UniqueUsername loginFormUsername)
  case mUser of
    Nothing -> throwError err401 -- Not 404, because then we leak data about users.
    Just (Entity _ User {..}) ->
      case checkPassword (mkPassword loginFormPassword) userPassword of
        PasswordCheckFail -> throwError err401
        PasswordCheckSuccess -> do
          let authCookie = AuthCookie {authCookieUsername = loginFormUsername}
          cookieSettings <- asks envCookieSettings
          jwtSettings <- asks envJWTSettings
          mCookie <- liftIO $ makeSessionCookieBS cookieSettings jwtSettings authCookie
          case mCookie of
            Nothing -> throwError err401
            Just setCookie -> return $ addHeader (TE.decodeUtf8 setCookie) NoContent
