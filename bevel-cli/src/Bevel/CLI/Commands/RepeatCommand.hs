{-# LANGUAGE TypeApplications #-}

module Bevel.CLI.Commands.RepeatCommand (repeatCommand, repeatCommandLoadSource) where

import Bevel.CLI.Env
import Bevel.CLI.Select
import Bevel.Client.Data
import Data.Word
import Database.Esqueleto.Experimental

repeatCommand :: C ()
repeatCommand =
  selectApp
    SelectAppSettings
      { selectAppSettingCount = repeatCommandCount,
        selectAppSettingLoadSource = repeatCommandLoadSource
      }

repeatCommandCount :: SqlPersistT IO Word64
repeatCommandCount = fmap (maybe 0 unValue) $
  selectOne $ do
    _ <- from $ table @ClientCommand
    pure countRows

repeatCommandLoadSource :: LoadSource
repeatCommandLoadSource = selectSource $ do
  clientCommand <- from $ table @ClientCommand
  -- New to old, because newer ones are the most important according to our formula
  orderBy [desc $ clientCommand ^. ClientCommandBegin]
  pure (clientCommand ^. ClientCommandBegin, clientCommand ^. ClientCommandText)
