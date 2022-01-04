{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bevel.API.Data where

import Bevel.API.Server.Data
import Bevel.Client.Data
import Bevel.Data
import Data.Aeson
import qualified Data.Appendful as Appendful
import Data.Functor.Contravariant
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import Servant.API.Generic
import Servant.Auth.Server

data RegistrationForm = RegistrationForm
  { registrationFormUsername :: Username,
    registrationFormPassword :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity RegistrationForm where
  validate rf@RegistrationForm {..} =
    mconcat
      [ genericValidate rf,
        declare "The password is nonempty" $ not $ T.null registrationFormPassword
      ]

instance ToJSON RegistrationForm where
  toJSON RegistrationForm {..} =
    object
      [ "name" .= registrationFormUsername,
        "password" .= registrationFormPassword
      ]

instance FromJSON RegistrationForm where
  parseJSON =
    withObject "RegistrationForm" $ \o ->
      RegistrationForm <$> o .: "name" <*> o .: "password"

data LoginForm = LoginForm
  { loginFormUsername :: Username,
    loginFormPassword :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity LoginForm

instance FromJSON LoginForm where
  parseJSON = withObject "LoginForm" $ \o ->
    LoginForm <$> o .: "username" <*> o .: "password"

instance ToJSON LoginForm where
  toJSON LoginForm {..} =
    object
      [ "username" .= loginFormUsername,
        "password" .= loginFormPassword
      ]

data AuthCookie = AuthCookie
  { authCookieUsername :: Username
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AuthCookie

instance ToJSON AuthCookie

instance FromJWT AuthCookie

instance ToJWT AuthCookie

data SyncRequest = SyncRequest
  { syncRequestCommandSyncRequest :: Appendful.SyncRequest ClientCommandId ServerCommandId Command
  }
  deriving (Show, Eq, Generic)

instance Validity SyncRequest

instance FromJSON SyncRequest where
  parseJSON = withObject "SyncResponse" $ \o ->
    SyncRequest
      <$> o .: "commands"

instance ToJSON SyncRequest where
  toJSON SyncRequest {..} =
    object
      [ "commands" .= syncRequestCommandSyncRequest
      ]

data SyncResponse = SyncResponse
  { syncResponseCommandSyncResponse :: Appendful.SyncResponse ClientCommandId ServerCommandId Command
  }
  deriving (Show, Eq, Generic)

instance Validity SyncResponse

instance FromJSON SyncResponse where
  parseJSON = withObject "SyncResponse" $ \o ->
    SyncResponse
      <$> o .: "commands"

instance ToJSON SyncResponse where
  toJSON SyncResponse {..} =
    object
      [ "commands" .= syncResponseCommandSyncResponse
      ]

instance (PersistEntity a, ToBackendKey SqlBackend a) => ToJSONKey (Key a) where
  toJSONKey = contramap fromSqlKey toJSONKey

instance (PersistEntity a, ToBackendKey SqlBackend a) => FromJSONKey (Key a) where
  fromJSONKey = toSqlKey <$> fromJSONKey
