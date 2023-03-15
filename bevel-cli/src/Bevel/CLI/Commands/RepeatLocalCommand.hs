{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Bevel.CLI.Commands.RepeatLocalCommand
  ( repeatLocalCommand,
    repeatLocalCommandLoadSource,
  )
where

import Bevel.CLI.Env
import Bevel.CLI.Select
import Bevel.Client.Data
import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Database.Esqueleto.Experimental
import Database.Esqueleto.Pagination
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
repeatLocalCommandLoadSource here =
  streamEntities (repeatLocalFilter here) ClientCommandBegin (PageSize 1024) Descend (Range Nothing Nothing)
    .| C.map (\(Entity _ ClientCommand {..}) -> (clientCommandBegin, clientCommandText))

repeatLocalWhereClauses :: Text -> SqlExpr (Entity ClientCommand) -> SqlQuery ()
repeatLocalWhereClauses here clientCommand =
  where_ $ repeatLocalFilter here clientCommand

repeatLocalFilter :: Text -> SqlExpr (Entity ClientCommand) -> SqlExpr (Value Bool)
repeatLocalFilter here clientCommand = clientCommand ^. ClientCommandWorkdir ==. val here
