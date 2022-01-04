{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bevel.Data.Thing where

import Data.Aeson
import Data.Validity
import GHC.Generics (Generic)

data Thing = Thing
  { thingNumber :: !Int
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Thing

instance FromJSON Thing where
  parseJSON = withObject "Thing" $ \o ->
    Thing
      <$> o .: "number"

instance ToJSON Thing where
  toJSON Thing {..} =
    object
      [ "number" .= thingNumber
      ]
