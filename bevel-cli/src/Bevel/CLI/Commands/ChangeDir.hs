{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Bevel.CLI.Commands.ChangeDir (changeDir, changeDirLoadSource) where

import Bevel.CLI.Env
import Bevel.CLI.Select
import Bevel.Client.Data
import Conduit
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Database.Esqueleto.Experimental
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
    whereClauses hostname username clientCommand
    pure countRows

changeDirLoadSource :: Text -> Text -> LoadSource
changeDirLoadSource hostname username = selectSource $ do
  clientCommand <- from $ table @ClientCommand
  whereClauses hostname username clientCommand
  orderBy [desc $ clientCommand ^. ClientCommandBegin]
  pure (clientCommand ^. ClientCommandBegin, castString $ clientCommand ^. ClientCommandWorkdir)

whereClauses :: Text -> Text -> SqlExpr (Entity ClientCommand) -> SqlQuery ()
whereClauses hostname username clientCommand = do
  where_ $ clientCommand ^. ClientCommandHost ==. val hostname
  where_ $ clientCommand ^. ClientCommandUser ==. val username
