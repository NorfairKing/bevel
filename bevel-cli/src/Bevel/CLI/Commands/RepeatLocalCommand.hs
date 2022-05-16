{-# LANGUAGE TypeApplications #-}

module Bevel.CLI.Commands.RepeatLocalCommand (repeatLocalCommand) where

import Bevel.CLI.Env
import Bevel.CLI.Select
import Bevel.Client.Data
import Database.Esqueleto.Experimental
import Path.IO

repeatLocalCommand :: C ()
repeatLocalCommand = do
  here <- getCurrentDir
  let whereClauses clientCommand =
        where_ $ clientCommand ^. ClientCommandWorkdir ==. val here

  selectApp
    SelectAppSettings
      { selectAppSettingCount = fmap (maybe 0 unValue) $
          selectOne $ do
            clientCommand <- from $ table @ClientCommand
            whereClauses clientCommand
            pure countRows,
        selectAppSettingLoadSource = selectSource $ do
          clientCommand <- from $ table @ClientCommand
          whereClauses clientCommand
          -- New to old, because newer ones are the most important according to our formula
          orderBy [desc $ clientCommand ^. ClientCommandBegin]
          pure (clientCommand ^. ClientCommandBegin, clientCommand ^. ClientCommandText)
      }
