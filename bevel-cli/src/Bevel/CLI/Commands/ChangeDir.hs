{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Bevel.CLI.Commands.ChangeDir
  ( changeDir,
    changeDirLoadSource,
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
import Network.HostName (getHostName)
import System.Posix.User (UserEntry (..), getRealUserID, getUserEntryForID)

changeDir :: C ()
changeDir = do
  hostname <- T.pack <$> liftIO getHostName
  username <- liftIO $ T.pack . userName <$> (getRealUserID >>= getUserEntryForID)
  selectApp
    SelectAppSettings
      { selectAppSettingCount = changeDirCount hostname username,
        selectAppSettingLoadSource = changeDirLoadSource hostname username
      }

changeDirCount :: Text -> Text -> SqlPersistT IO Word64
changeDirCount hostname username = fmap (maybe 0 unValue) $
  selectOne $ do
    clientCommand <- from $ table @ClientCommand
    changeDirWhereClauses hostname username clientCommand
    pure countRows

changeDirLoadSource :: Text -> Text -> LoadSource
changeDirLoadSource hostname username =
  streamEntities (changeDirFilter hostname username) ClientCommandBegin (PageSize 1024) Descend (Range Nothing Nothing)
    .| C.map (\(Entity _ ClientCommand {..}) -> (clientCommandBegin, clientCommandWorkdir))

changeDirWhereClauses :: Text -> Text -> SqlExpr (Entity ClientCommand) -> SqlQuery ()
changeDirWhereClauses hostname username clientCommand =
  where_ $ changeDirFilter hostname username clientCommand

changeDirFilter :: Text -> Text -> SqlExpr (Entity ClientCommand) -> SqlExpr (Value Bool)
changeDirFilter hostname username clientCommand =
  (clientCommand ^. ClientCommandHost ==. val hostname)
    &&. (clientCommand ^. ClientCommandUser ==. val username)
