module Bevel.API.Server.SigningKey
  ( loadSigningKey,
  )
where

import Crypto.JOSE.JWK (JWK)
import Path
import Path.IO
import Servant.Auth.Server as Auth

loadSigningKey :: Path Abs File -> IO JWK
loadSigningKey skf = do
  mErrOrKey <- forgivingAbsence $ readKey (toFilePath skf)
  case mErrOrKey of
    Nothing -> do
      writeKey (fromAbsFile skf)
      readKey (fromAbsFile skf)
    Just r -> pure r
