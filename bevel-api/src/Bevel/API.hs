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
    postDownload :: !(route :- PostDownload),
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

-- | Download the commands that the server already has, in batches.
--
-- Appendful's sync is fine for small deltas, but its server-side read is
-- unbounded, so the first sync on a new machine would have to fit every
-- command the server has into a single response.
type PostDownload =
  ProtectAPI
    :> "download"
    :> ReqBody '[JSON] DownloadRequest
    :> Post '[JSON] DownloadResponse

type PostSync =
  ProtectAPI
    :> "sync"
    :> ReqBody '[JSON] SyncRequest
    :> Get '[JSON] SyncResponse
