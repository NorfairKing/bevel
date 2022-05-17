{-# LANGUAGE TypeApplications #-}

module Bevel.CLI.Commands.RepeatLocalCommand (repeatLocalCommand) where

import Bevel.CLI.Env
import Bevel.CLI.Select
import Bevel.Client.Data
import Data.Word
import Database.Esqueleto.Experimental
import Path
import Path.IO

repeatLocalCommand :: C ()
repeatLocalCommand = do
  here <- getCurrentDir
  selectApp
    SelectAppSettings
      { selectAppSettingCount = repeatLocalCommandCount here,
        selectAppSettingLoadSource = repeatLocalCommandLoadSource here
      }

repeatLocalCommandCount :: Path Abs Dir -> SqlPersistT IO Word64
repeatLocalCommandCount here = fmap (maybe 0 unValue) $
  selectOne $ do
    clientCommand <- from $ table @ClientCommand
    repeatLocalWhereClauses here clientCommand
    pure countRows

repeatLocalCommandLoadSource :: Path Abs Dir -> LoadSource
repeatLocalCommandLoadSource here = selectSource $ do
  clientCommand <- from $ table @ClientCommand
  repeatLocalWhereClauses here clientCommand
  -- New to old, because newer ones are the most important according to our formula
  orderBy [desc $ clientCommand ^. ClientCommandBegin]
  pure (clientCommand ^. ClientCommandBegin, clientCommand ^. ClientCommandText)

repeatLocalWhereClauses :: Path Abs Dir -> SqlExpr (Entity ClientCommand) -> SqlQuery ()
repeatLocalWhereClauses here clientCommand =
  where_ $ clientCommand ^. ClientCommandWorkdir ==. val here
