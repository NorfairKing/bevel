{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Bevel.CLI.Commands.ChangeDir (changeDir) where

import Bevel.CLI.Env
import Bevel.CLI.Select
import Bevel.Client.Data
import Conduit
import qualified Data.Text as T
import Database.Esqueleto.Experimental
import Network.HostName (getHostName)
import System.Posix.User (UserEntry (..), getRealUserID, getUserEntryForID)

changeDir :: C ()
changeDir = do
  hostname <- liftIO getHostName
  username <- liftIO $ userName <$> (getRealUserID >>= getUserEntryForID)
  let whereClauses clientCommand = do
        where_ $ clientCommand ^. ClientCommandHost ==. val (T.pack hostname)
        where_ $ clientCommand ^. ClientCommandUser ==. val (T.pack username)
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
          orderBy [desc $ clientCommand ^. ClientCommandBegin]
          pure (clientCommand ^. ClientCommandBegin, castString $ clientCommand ^. ClientCommandWorkdir)
      }
