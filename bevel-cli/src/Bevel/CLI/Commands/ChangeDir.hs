{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bevel.CLI.Commands.ChangeDir (changeDir) where

import Bevel.CLI.Env
import Bevel.Client.Data
import Brick.AttrMap
import Brick.BChan
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Core
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Cursor.Brick.List.NonEmpty
import Cursor.Simple.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Set as S
import Database.Persist
import Graphics.Vty (defaultConfig, mkVty, outputFd)
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Path
import System.Posix.IO.ByteString (stdError)

changeDir :: C ()
changeDir = do
  initialState <- buildInitialState
  liftIO $ do
    reqChan <- newBChan 1000
    respChan <- newBChan 1000
    let vtyBuilder = mkVty $ defaultConfig {outputFd = Just stdError}
    firstVty <- vtyBuilder
    let runTui = customMain firstVty vtyBuilder (Just respChan) (tuiApp reqChan) initialState
    let workerEnv = WorkerEnv
    let runWorker = runReaderT (tuiWorker reqChan respChan) workerEnv
    -- Left always works because the worker runs forever
    Left endState <- race runTui runWorker
    forM_ (stateDirs endState) $ \nec ->
      putStrLn $ fromAbsDir $ nonEmptyCursorCurrent nec

tuiApp :: BChan Request -> App State Response ResourceName
tuiApp chan =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent chan,
      appStartEvent = pure,
      appAttrMap = buildAttrMap
    }

data State = State
  { stateDirs :: Maybe (NonEmptyCursor (Path Abs Dir))
  }
  deriving (Show)

data ResourceName = ResourceName
  deriving (Show, Eq, Ord)

buildInitialState :: C State
buildInitialState = do
  dirs <-
    runDB $
      S.toList . S.fromList . map (\(Entity _ ClientCommand {..}) -> clientCommandWorkdir)
        <$> selectList [] []
  let stateDirs = makeNonEmptyCursor <$> NE.nonEmpty dirs
  pure State {..}

buildAttrMap :: State -> AttrMap
buildAttrMap =
  const $
    attrMap
      defAttr
      [ (selectedAttr, fg white)
      ]

selectedAttr :: AttrName
selectedAttr = "selected"

drawTui :: State -> [Widget ResourceName]
drawTui State {..} =
  [ case stateDirs of
      Nothing -> str "Empty"
      Just dirs ->
        let goDir = str . fromAbsDir
         in verticalNonEmptyCursorWidget
              goDir
              (withAttr selectedAttr . goDir)
              goDir
              dirs
  ]

handleTuiEvent :: BChan Request -> State -> BrickEvent n Response -> EventM n (Next State)
handleTuiEvent _ s e =
  case e of
    VtyEvent vtye ->
      let modList func = continue $ s {stateDirs = func <$> stateDirs s}
          modMList mFunc = modList $ \nec -> fromMaybe nec $ mFunc nec
       in case vtye of
            EvKey KEnter [] -> halt s
            EvKey KDown [] -> modMList nonEmptyCursorSelectNext
            EvKey KUp [] -> modMList nonEmptyCursorSelectPrev
            EvKey KLeft [] -> modList nonEmptyCursorSelectFirst
            EvKey KRight [] -> modList nonEmptyCursorSelectLast
            _ -> continue s
    AppEvent resp -> case resp of
      Response -> continue s
    _ -> continue s

data WorkerEnv = WorkerEnv
  {
  }

type W = ReaderT WorkerEnv IO

data Request = Request

data Response = Response

tuiWorker :: BChan Request -> BChan Response -> W ()
tuiWorker reqChan respChan = forever $ do
  req <- liftIO $ readBChan reqChan
  resp <- case req of
    Request -> pure Response
  liftIO $ writeBChan respChan resp
