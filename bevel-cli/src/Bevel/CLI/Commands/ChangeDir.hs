{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Bevel.CLI.Commands.ChangeDir (changeDir) where

import Bevel.CLI.Choices
import Bevel.CLI.Env
import Bevel.CLI.Search
import Bevel.Client.Data
import Brick.AttrMap
import Brick.BChan
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Core
import Conduit
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Reader
import Cursor.Brick.List.NonEmpty
import Cursor.Brick.Text
import Cursor.Simple.List.NonEmpty
import Cursor.Text
import Cursor.Types
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.Ord as Ord
import qualified Data.Text as T
import Data.Time
import Database.Esqueleto.Experimental
import qualified Database.Persist.Sql as DB
import Graphics.Vty (defaultConfig, mkVty, outputFd)
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Network.HostName (getHostName)
import Path
import System.Exit
import System.Posix.IO.ByteString (stdError)
import System.Posix.User (UserEntry (..), getRealUserID, getUserEntryForID)

changeDir :: C ()
changeDir = do
  initialState <- buildInitialState
  workerEnvConnectionPool <- asks envConnectionPool
  liftIO $ do
    reqChan <- newBChan 1000
    respChan <- newBChan 1000
    let vtyBuilder = mkVty $ defaultConfig {outputFd = Just stdError}
    firstVty <- vtyBuilder
    let runTui = customMain firstVty vtyBuilder (Just respChan) (tuiApp reqChan) initialState
    let workerEnv = WorkerEnv {..}
    let runWorker = runReaderT (tuiWorker reqChan respChan) workerEnv
    -- Left always works because the worker runs forever
    Left endState <- race runTui runWorker
    if stateDone endState
      then do
        forM_ (stateOptions endState) $ \nec ->
          putStrLn $ fromAbsDir $ nonEmptyCursorCurrent nec
      else exitFailure

tuiApp :: BChan Request -> App State Response ResourceName
tuiApp chan =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent chan,
      appStartEvent = \s -> do
        liftIO $ writeBChan chan RequestLoad
        pure s,
      appAttrMap = buildAttrMap
    }

data State = State
  { stateChoices :: !(Choices (Path Abs Dir)),
    stateOptions :: !(Maybe (NonEmptyCursor (Path Abs Dir))),
    stateSearch :: !TextCursor,
    stateDone :: !Bool
  }
  deriving (Show)

data ResourceName = SearchBox | OptionsViewport
  deriving (Show, Eq, Ord)

buildInitialState :: C State
buildInitialState = do
  let stateChoices = mempty
  let stateSearch = emptyTextCursor
  let stateOptions = refreshOptions stateChoices stateSearch
  let stateDone = False
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
  [ padLeftRight 1 $
      vBox
        [ viewport OptionsViewport Vertical $
            vLimit 1024 $ -- Arbitrary "big" limit to make the widget fixed-size (somehow that's faster)
              padTop Max $ case stateOptions of
                Nothing -> str "Empty"
                Just dirs ->
                  let goDir = str . fromAbsDir
                   in nonEmptyCursorWidget
                        ( \befores current afters ->
                            vBox $
                              reverse $
                                concat [map goDir befores, [visible $ withAttr selectedAttr $ goDir current], map goDir afters]
                        )
                        dirs,
          vLimit 1 $ withAttr selectedAttr $ selectedTextCursorWidget SearchBox stateSearch
        ]
  ]

handleTuiEvent :: BChan Request -> State -> BrickEvent n Response -> EventM n (Next State)
handleTuiEvent _ s e =
  case e of
    VtyEvent vtye ->
      let modOptions func = continue $ s {stateOptions = func <$> stateOptions s}
          modMOptions mFunc = modOptions $ \nec -> fromMaybe nec $ mFunc nec
          modSearch func = do
            let newSearch = func $ stateSearch s
            continue $
              s
                { stateSearch = newSearch,
                  stateOptions = refreshOptions (stateChoices s) newSearch
                }
          modMSearch mFunc = modSearch $ \tc -> fromMaybe tc $ mFunc tc
       in case vtye of
            EvKey KEnter [] -> halt s {stateDone = True}
            EvKey KEsc [] -> halt s {stateDone = False}
            EvKey KDown [] -> modMOptions nonEmptyCursorSelectPrev
            EvKey KUp [] -> modMOptions nonEmptyCursorSelectNext
            EvKey (KChar c) [] -> modMSearch $ textCursorInsert c
            EvKey KBS [] -> modMSearch $ dullMDelete . textCursorRemove
            EvKey KDel [] -> modMSearch $ dullMDelete . textCursorDelete
            EvKey KLeft [] -> modMSearch textCursorSelectPrev
            EvKey KRight [] -> modMSearch textCursorSelectNext
            _ -> continue s
    AppEvent resp -> case resp of
      ResponsePartialLoad additionalChoices -> do
        let newChoices = stateChoices s <> additionalChoices
        continue $
          s
            { stateChoices = newChoices,
              stateOptions = refreshOptions newChoices (stateSearch s)
            }
    _ -> continue s

data WorkerEnv = WorkerEnv
  { workerEnvConnectionPool :: DB.ConnectionPool
  }

type W = ReaderT WorkerEnv IO

data Request = RequestLoad

data Response = ResponsePartialLoad !(Choices (Path Abs Dir))

tuiWorker :: BChan Request -> BChan Response -> W ()
tuiWorker reqChan respChan = forever $
  runResourceT $ do
    req <- liftIO $ readBChan reqChan
    case req of
      RequestLoad -> do
        now <- liftIO getCurrentTime
        hostname <- liftIO getHostName
        username <- liftIO $ userName <$> (getRealUserID >>= getUserEntryForID)
        let source = selectSource $ do
              clientCommand <- from $ table @ClientCommand
              where_ $ clientCommand ^. ClientCommandHost ==. val (T.pack hostname)
              where_ $ clientCommand ^. ClientCommandUser ==. val (T.pack username)
              orderBy [desc $ clientCommand ^. ClientCommandBegin]
              pure (clientCommand ^. ClientCommandBegin, clientCommand ^. ClientCommandWorkdir)
        pool <- asks workerEnvConnectionPool
        flip runSqlPool pool $
          runConduit $
            source
              .| C.map (\(Value time, Value dir) -> (time, dir))
              .| CL.chunksOf 1024
              .| C.map (makeChoices now)
              .| C.mapM_
                ( \s ->
                    liftIO $
                      writeBChan respChan $ ResponsePartialLoad s
                )

refreshOptions :: Choices (Path Abs Dir) -> TextCursor -> Maybe (NonEmptyCursor (Path Abs Dir))
refreshOptions cs search =
  let query = rebuildTextCursor search
      newOptions =
        map (snd . fst)
          . sortOn (\((fuzziness, _), score) -> Ord.Down (fuzziness, score))
          . filter ((> 0) . fst . fst)
          . map (\(dir, score) -> ((fuzzySearch query (T.pack (fromAbsDir dir)), dir), score))
          . sortOn (Ord.Down . snd)
          $ M.toList $ choicesMap cs
   in makeNonEmptyCursor <$> NE.nonEmpty newOptions
