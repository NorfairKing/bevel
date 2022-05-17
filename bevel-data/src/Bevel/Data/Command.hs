{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Bevel.Data.Command where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Int
import Data.Text (Text)
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Word
import GHC.Generics (Generic)

data Command = Command
  { commandText :: !Text,
    commandBegin :: !Word64,
    commandEnd :: !(Maybe Word64),
    commandWorkdir :: !Text,
    commandUser :: !Text,
    commandHost :: !Text,
    commandExit :: !(Maybe Int8)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Command)

instance Validity Command

instance HasCodec Command where
  codec =
    object "Command" $
      Command
        <$> requiredField "text" "the command itself" .= commandText
        <*> requiredField "begin" "start of execution (nanoseconds since 1970)" .= commandBegin
        <*> requiredField "end" "end of execution (nanoseconds since 1970)" .= commandEnd
        <*> requiredField "workdir" "working directory" .= commandWorkdir
        <*> requiredField "user" "user that ran the command" .= commandUser
        <*> requiredField "host" "host on which the command was run" .= commandHost
        <*> requiredField "exit" "exit code" .= commandExit
