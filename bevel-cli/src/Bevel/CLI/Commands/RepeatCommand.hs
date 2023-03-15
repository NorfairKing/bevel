{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Bevel.CLI.Commands.RepeatCommand (repeatCommand, repeatCommandLoadSource) where

import Bevel.CLI.Env
import Bevel.CLI.Select
import Bevel.Client.Data
import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Word
import Database.Esqueleto.Experimental
import Database.Esqueleto.Pagination

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
repeatCommandLoadSource =
  streamEntities (\_ -> val True) ClientCommandBegin (PageSize 1024) Descend (Range Nothing Nothing)
    .| C.map (\(Entity _ ClientCommand {..}) -> (clientCommandBegin, clientCommandText))
