{-# LANGUAGE TypeApplications #-}

module Bevel.CLI.Commands.RepeatLocalCommand
  ( repeatLocalCommand,
    repeatLocalCommandLoadSource,
  )
where

import Bevel.CLI.Env
import Bevel.CLI.Select
import Bevel.Client.Data
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Database.Esqueleto.Experimental
import Path
import Path.IO
import qualified System.FilePath as FP

repeatLocalCommand :: C ()
repeatLocalCommand = do
  here <- T.pack . FP.dropTrailingPathSeparator . fromAbsDir <$> getCurrentDir
  selectApp
    SelectAppSettings
      { selectAppSettingCount = repeatLocalCommandCount here,
        selectAppSettingLoadSource = repeatLocalCommandLoadSource here
      }

repeatLocalCommandCount :: Text -> SqlPersistT IO Word64
repeatLocalCommandCount here = fmap (maybe 0 unValue) $
  selectOne $ do
    clientCommand <- from $ table @ClientCommand
    repeatLocalWhereClauses here clientCommand
    pure countRows

repeatLocalCommandLoadSource :: Text -> LoadSource
repeatLocalCommandLoadSource here = selectSource $ do
  clientCommand <- from $ table @ClientCommand
  repeatLocalWhereClauses here clientCommand
  -- New to old, because newer ones are the most important according to our formula
  orderBy [desc $ clientCommand ^. ClientCommandBegin]
  pure (clientCommand ^. ClientCommandBegin, clientCommand ^. ClientCommandText)

repeatLocalWhereClauses :: Text -> SqlExpr (Entity ClientCommand) -> SqlQuery ()
repeatLocalWhereClauses here clientCommand =
  where_ $ clientCommand ^. ClientCommandWorkdir ==. val here
