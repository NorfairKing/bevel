{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Bevel.Data.Command where

import Autodocodec
import Control.Arrow (left)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Word
import GHC.Generics (Generic)
import Path

data Command = Command
  { commandText :: !Text,
    commandBegin :: !Word64,
    commandEnd :: !(Maybe Word64),
    commandWorkdir :: !(Path Abs Dir),
    commandUser :: !Text,
    commandHost :: !Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Command)

instance Validity Command

instance HasCodec Command where
  codec =
    object "Command" $
      Command
        <$> requiredField "text" "the command itself" .= commandText
        <*> requiredField "begin" "start of execution (microseconds since 1970)" .= commandBegin
        <*> requiredField "end" "end of execution (microseconds since 1970)" .= commandEnd
        <*> requiredFieldWith "workdir" (bimapCodec (left show . parseAbsDir) fromAbsDir codec) "working directory" .= commandWorkdir
        <*> requiredField "user" "user that ran the command" .= commandUser
        <*> requiredField "host" "host on which the command was run" .= commandHost
