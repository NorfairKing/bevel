{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Bevel.API.Server.Data.Username where

import Autodocodec
import Control.Arrow (left)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)

newtype Username = Username
  { usernameText :: Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Username)

instance Validity Username where
  validate (Username t) =
    mconcat
      [ check (not (T.null t)) "The username is not empty.",
        check (T.length t >= 3) "The username is at least three characters long."
      ]

instance PersistField Username where
  toPersistValue = toPersistValue . usernameText
  fromPersistValue pv = do
    t <- fromPersistValue pv
    left T.pack $ parseUsernameOrErr t

instance PersistFieldSql Username where
  sqlType _ = SqlString

instance HasCodec Username where
  codec = bimapCodec parseUsernameOrErr usernameText codec

parseUsernameOrErr :: Text -> Either String Username
parseUsernameOrErr = prettyValidate . Username
