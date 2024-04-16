{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Bevel.API where

import Bevel.API.Data
import Data.Text (Text)
import Data.Validity.Text ()
import Servant.API
import Servant.API.Generic
import Servant.Auth

data BevelRoutes route = BevelRoutes
  { postRegister :: !(route :- PostRegister),
    postLogin :: !(route :- PostLogin),
    postSync :: !(route :- PostSync)
  }
  deriving (Generic)

type PostRegister =
  "register"
    :> ReqBody '[JSON] RegistrationForm
    :> PostNoContent

type PostLogin =
  "login"
    :> ReqBody '[JSON] LoginForm
    :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)

type ProtectAPI = Auth '[JWT] AuthCookie

type PostSync =
  ProtectAPI
    :> "sync"
    :> ReqBody '[JSON] SyncRequest
    :> Get '[JSON] SyncResponse
