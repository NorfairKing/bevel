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
import Cursor.Brick.Text
import Cursor.Simple.List.NonEmpty
import Cursor.Text
import Cursor.Types
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
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
    forM_ (stateOptions endState) $ \nec ->
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
  { stateDirs :: Set (Path Abs Dir),
    stateOptions :: !(Maybe (NonEmptyCursor (Path Abs Dir))),
    stateSearch :: !TextCursor
  }
  deriving (Show)

data ResourceName = SearchBox
  deriving (Show, Eq, Ord)

buildInitialState :: C State
buildInitialState = do
  dirs <- runDB $ S.fromList . map (\(Entity _ ClientCommand {..}) -> clientCommandWorkdir) <$> selectList [] []
  let stateDirs = dirs
  let stateOptions = makeNonEmptyCursor <$> NE.nonEmpty (S.toList dirs)
  let stateSearch = emptyTextCursor
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
  [ vBox
      [ padTop Max $ case stateOptions of
          Nothing -> str "Empty"
          Just dirs ->
            let goDir = str . fromAbsDir
             in nonEmptyCursorWidget
                  ( \befores current afters ->
                      vBox $
                        reverse $
                          concat [map goDir befores, [withAttr selectedAttr $ goDir current], map goDir afters]
                  )
                  dirs,
        visible $ withAttr selectedAttr $ selectedTextCursorWidget SearchBox stateSearch
      ]
  ]

fuzzySearch :: Text -> Path Abs Dir -> Bool
fuzzySearch query path = null $ T.foldl' go (T.unpack (T.toCaseFold query)) (T.toCaseFold (T.pack (fromAbsDir path)))
  where
    go :: [Char] -> Char -> [Char]
    go [] _ = []
    go q@(qc : rest) dc
      | qc == dc = rest
      | otherwise = q

handleTuiEvent :: BChan Request -> State -> BrickEvent n Response -> EventM n (Next State)
handleTuiEvent _ s e =
  case e of
    VtyEvent vtye ->
      let modOptions func = continue $ s {stateOptions = func <$> stateOptions s}
          modMOptions mFunc = modOptions $ \nec -> fromMaybe nec $ mFunc nec
          modSearch func = do
            let newSearch = func $ stateSearch s
            let query = rebuildTextCursor newSearch
            let newOptions = S.toList $ S.filter (fuzzySearch query) (stateDirs s)
            let newOptionsCursor = makeNonEmptyCursor <$> NE.nonEmpty newOptions
            continue $ s {stateSearch = newSearch, stateOptions = newOptionsCursor}
          modMSearch mFunc = modSearch $ \tc -> fromMaybe tc $ mFunc tc
       in case vtye of
            EvKey KEnter [] -> halt s
            EvKey KDown [] -> modMOptions nonEmptyCursorSelectPrev
            EvKey KUp [] -> modMOptions nonEmptyCursorSelectNext
            EvKey (KChar c) [] -> modMSearch $ textCursorInsert c
            EvKey KBS [] -> modMSearch $ dullMDelete . textCursorRemove
            EvKey KDel [] -> modMSearch $ dullMDelete . textCursorDelete
            EvKey KLeft [] -> modMSearch textCursorSelectPrev
            EvKey KRight [] -> modMSearch textCursorSelectNext
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
