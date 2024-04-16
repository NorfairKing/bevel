{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bevel.API.Data where

import Autodocodec
import Bevel.API.Server.Data
import Bevel.Client.Data
import Bevel.Data
import Data.Aeson (FromJSON, FromJSONKey (..), ToJSON, ToJSONKey (..))
import qualified Data.Appendful as Appendful
import Data.Functor.Contravariant
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql as Sql
import Servant.API.Generic
import Servant.Auth.Server

data RegistrationForm = RegistrationForm
  { registrationFormUsername :: Username,
    registrationFormPassword :: Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec RegistrationForm)

instance Validity RegistrationForm where
  validate rf@RegistrationForm {..} =
    mconcat
      [ genericValidate rf,
        declare "The password is nonempty" $ not $ T.null registrationFormPassword
      ]

instance HasCodec RegistrationForm where
  codec =
    object "RegistrationForm" $
      RegistrationForm
        <$> requiredField "username" "user name"
          .= registrationFormUsername
        <*> requiredField "password" "password"
          .= registrationFormPassword

data LoginForm = LoginForm
  { loginFormUsername :: Username,
    loginFormPassword :: Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec LoginForm)

instance Validity LoginForm

instance HasCodec LoginForm where
  codec =
    object "LoginForm" $
      LoginForm
        <$> requiredField "username" "user name"
          .= loginFormUsername
        <*> requiredField "password" "password"
          .= loginFormPassword

data AuthCookie = AuthCookie
  { authCookieUsername :: Username
  }
  deriving (Generic)

instance FromJSON AuthCookie

instance ToJSON AuthCookie

instance FromJWT AuthCookie

instance ToJWT AuthCookie

data SyncRequest = SyncRequest
  { syncRequestCommandSyncRequest :: Appendful.SyncRequest ClientCommandId ServerCommandId Command
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec SyncRequest)

instance Validity SyncRequest

instance HasCodec SyncRequest where
  codec =
    object "SyncRequest" $
      SyncRequest <$> requiredField "command" "commands sync request" .= syncRequestCommandSyncRequest

data SyncResponse = SyncResponse
  { syncResponseCommandSyncResponse :: Appendful.SyncResponse ClientCommandId ServerCommandId Command
  }
  deriving (FromJSON, ToJSON) via (Autodocodec SyncResponse)

instance HasCodec SyncResponse where
  codec =
    object "SyncResponse" $
      SyncResponse <$> requiredField "command" "commands sync response" .= syncResponseCommandSyncResponse

instance (ToBackendKey SqlBackend a) => HasCodec (Sql.Key a) where
  codec = dimapCodec toSqlKey fromSqlKey codec

instance (ToBackendKey SqlBackend a) => ToJSONKey (Sql.Key a) where
  toJSONKey = contramap fromSqlKey toJSONKey

instance (ToBackendKey SqlBackend a) => FromJSONKey (Sql.Key a) where
  fromJSONKey = toSqlKey <$> fromJSONKey
