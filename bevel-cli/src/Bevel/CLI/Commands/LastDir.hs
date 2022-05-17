{-# LANGUAGE TypeApplications #-}

module Bevel.CLI.Commands.LastDir where

import Bevel.CLI.Commands.Import
import qualified Data.Text as T
import Database.Esqueleto.Experimental

lastDir :: C ()
lastDir = do
  mLastDir <- runDB $
    selectOne $ do
      clientCommand <- from $ table @ClientCommand
      offset 1 -- So we don't match on the just-run bevel-last command but rather the one before
      orderBy [desc $ clientCommand ^. ClientCommandBegin]
      pure $ clientCommand ^. ClientCommandWorkdir
  forM_ mLastDir $ \(Value dir) ->
    liftIO $ putStrLn $ T.unpack dir
