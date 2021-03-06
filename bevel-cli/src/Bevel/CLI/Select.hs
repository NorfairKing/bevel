{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bevel.CLI.Select
  ( SelectAppSettings (..),
    LoadSource,
    selectApp,
    loadChoices,
  )
where

import Bevel.CLI.Choices
import Bevel.CLI.Env
import Bevel.CLI.Score
import Bevel.CLI.Search
import Brick.AttrMap
import Brick.BChan
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Core
import Conduit
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
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Word
import Database.Esqueleto.Experimental
import qualified Database.Persist.Sql as DB
import Graphics.Vty (defaultConfig, mkVty, outputFd)
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import System.Exit
import System.Posix.IO.ByteString (stdError)
import Text.Printf
import UnliftIO

data SelectAppSettings = SelectAppSettings
  { selectAppSettingCount :: !(SqlPersistT IO Word64),
    selectAppSettingLoadSource :: !LoadSource
  }

type LoadSource = ConduitT () (Value Word64, Value Text) (SqlPersistT (ResourceT IO)) ()

selectApp :: SelectAppSettings -> C ()
selectApp settings = do
  initialState <- buildInitialState settings
  workerEnvConnectionPool <- asks envConnectionPool
  maxOptions <- asks envMaxOptions
  liftIO $ do
    reqChan <- newBChan 1000
    respChan <- newBChan 1000
    let vtyBuilder = mkVty $ defaultConfig {outputFd = Just stdError}
    firstVty <- vtyBuilder
    let runTui = customMain firstVty vtyBuilder (Just respChan) (tuiApp maxOptions reqChan) initialState
    let workerEnv = WorkerEnv {..}
    let runWorker = runReaderT (tuiWorker settings reqChan respChan) workerEnv
    -- Left always works because the worker runs forever
    Left endState <- race runTui runWorker
    if stateDone endState
      then do
        forM_ (stateOptions endState) $ \nec ->
          putStrLn $ T.unpack $ nonEmptyCursorCurrent nec
      else exitFailure

tuiApp :: Word8 -> BChan Request -> App State Response ResourceName
tuiApp maxOptions chan =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent maxOptions chan,
      appStartEvent = \s -> do
        liftIO $ writeBChan chan $ RequestLoad T.empty -- Empty query
        pure s,
      appAttrMap = buildAttrMap
    }

data State = State
  { stateTotal :: !Word64,
    stateChoices :: !Choices,
    stateOptions :: !(Maybe (NonEmptyCursor Text)),
    stateSearch :: !TextCursor,
    stateDebug :: !Bool,
    stateDone :: !Bool
  }
  deriving (Show)

data ResourceName = SearchBox | OptionsViewport
  deriving (Show, Eq, Ord)

buildInitialState :: SelectAppSettings -> C State
buildInitialState SelectAppSettings {..} = do
  stateTotal <- runDB selectAppSettingCount
  let stateChoices = mempty
  let stateSearch = emptyTextCursor
  let stateOptions = Nothing
  let stateDebug = False
  let stateDone = False
  pure State {..}

buildAttrMap :: State -> AttrMap
buildAttrMap =
  const $
    attrMap
      (fg yellow)
      [ (selectedAttr, fg white),
        (loadedAttr, fg green),
        (unloadedAttr, fg red)
      ]

selectedAttr :: AttrName
selectedAttr = "selected"

loadedAttr :: AttrName
loadedAttr = "loaded"

unloadedAttr :: AttrName
unloadedAttr = "unloaded"

drawTui :: State -> [Widget ResourceName]
drawTui State {..} =
  [ case stateOptions of
      Nothing -> emptyWidget
      Just dirs ->
        let goCommand selected command =
              ( if stateDebug
                  then
                    ( \w ->
                        let (fuzziness, score) = lookupChoice stateChoices command
                         in hBox
                              [ w,
                                padLeft Max $ str $ printf "%6.0f" $ unFuzziness fuzziness,
                                padLeft Max $ str $ printf "%6.0f" $ unScore score
                              ]
                    )
                  else id
              )
                $ hBox
                  [ str $
                      if selected
                        then "??? "
                        else "  ",
                    txt command
                  ]
         in nonEmptyCursorWidget
              ( \befores current afters ->
                  let maxHeight = sum . map (length . T.lines) . concat $ [befores, [current], afters]
                   in padLeftRight 1 $
                        vBox
                          [ padTop Max
                              . vLimit maxHeight
                              . viewport OptionsViewport Vertical
                              . vBox
                              . reverse
                              . concat
                              $ [ map (goCommand False) befores,
                                  [visible $ withAttr selectedAttr $ goCommand True current],
                                  map (goCommand False) afters
                                ],
                            vLimit 1 $
                              hBox
                                [ withAttr selectedAttr $ selectedTextCursorWidget SearchBox stateSearch,
                                  padLeft Max $
                                    let currentProcessed = choicesTotal stateChoices
                                        totalDigits :: Int
                                        totalDigits = ceiling (logBase 10 $ fromIntegral stateTotal :: Double)
                                        formatStr :: String
                                        formatStr = "%" <> show totalDigits <> "d"
                                     in hBox
                                          [ withAttr
                                              ( if currentProcessed < stateTotal
                                                  then unloadedAttr
                                                  else loadedAttr
                                              )
                                              . str
                                              $ printf formatStr currentProcessed,
                                            str " / ",
                                            withAttr loadedAttr $ str $ printf formatStr stateTotal
                                          ]
                                ]
                          ]
              )
              dirs
  ]

handleTuiEvent :: Word8 -> BChan Request -> State -> BrickEvent n Response -> EventM n (Next State)
handleTuiEvent maxOptions chan s e =
  case e of
    VtyEvent vtye ->
      let modOptions func = continue $ s {stateOptions = func <$> stateOptions s}
          modMOptions mFunc = modOptions $ \nec -> fromMaybe nec $ mFunc nec
          modSearch func = do
            -- Update the search bar
            let newSearch = func $ stateSearch s
            -- Reset the choices
            let newChoices = mempty
            let newOptions = Nothing
            liftIO $ writeBChan chan $ RequestLoad $ rebuildTextCursor newSearch
            -- Update the state
            continue $
              s
                { stateSearch = newSearch,
                  stateChoices = newChoices,
                  stateOptions = newOptions
                }
          modMSearch mFunc = modSearch $ \tc -> fromMaybe tc $ mFunc tc
       in case vtye of
            EvKey KEnter [] -> halt s {stateDone = True}
            EvKey KEsc [] -> halt s {stateDone = False}
            EvKey KDown [] -> modMOptions nonEmptyCursorSelectPrev
            EvKey KUp [] -> modMOptions nonEmptyCursorSelectNext
            EvKey (KChar c) [] -> modMSearch $ textCursorInsert c
            EvKey (KChar 'd') [MMeta] -> continue s {stateDebug = not $ stateDebug s}
            EvKey KBS [] -> modMSearch $ dullMDelete . textCursorRemove
            EvKey KDel [] -> modMSearch $ dullMDelete . textCursorDelete
            EvKey KLeft [] -> modMSearch textCursorSelectPrev
            EvKey KRight [] -> modMSearch textCursorSelectNext
            _ -> continue s
    AppEvent resp -> case resp of
      ResponsePartialLoad queryAtTheTime additionalChoices ->
        -- Ignore any loading for queries that aren't relevant anymore.
        if queryAtTheTime == rebuildTextCursor (stateSearch s)
          then do
            let newChoices = stateChoices s <> additionalChoices
            continue $
              s
                { stateChoices = newChoices,
                  stateOptions = refreshOptions maxOptions (stateOptions s) newChoices
                }
          else continue s
    _ -> continue s

data WorkerEnv = WorkerEnv
  { workerEnvConnectionPool :: DB.ConnectionPool
  }

type W = ReaderT WorkerEnv IO

data Request
  = -- | Request a reload with the current query
    RequestLoad !Text

data Response
  = -- | A bunch of choices that were loaded for the given query
    ResponsePartialLoad !Text !Choices

tuiWorker :: SelectAppSettings -> BChan Request -> BChan Response -> W ()
tuiWorker sets reqChan respChan = do
  -- A variable that holds the filterer thread, so we can reset it asap.
  filtererVar <- newEmptyMVar
  forever $ do
    req <- liftIO $ readBChan reqChan
    case req of
      RequestLoad query -> do
        -- Stop the current filterer job, if there is one.
        mFiltererJob <- tryTakeMVar filtererVar
        mapM_ cancel mFiltererJob
        -- Start a new filterer job
        filtererAsync <- async $ filtererJob sets respChan query
        putMVar filtererVar filtererAsync
        pure () :: W ()

filtererJob :: SelectAppSettings -> BChan Response -> Text -> W ()
filtererJob SelectAppSettings {..} respChan query = runResourceT $ do
  now <- liftIO getCurrentTime
  pool <- asks workerEnvConnectionPool
  liftIO $
    runResourceT $
      flip runSqlPool pool $
        runConduit $
          loadChoices selectAppSettingLoadSource now query
            .| C.mapM_
              ( \s -> do
                  -- Fully evaluate the response first
                  let !load = ResponsePartialLoad query s
                  -- Then send it to the UI
                  liftIO $ writeBChan respChan load
              )

loadChoices :: LoadSource -> UTCTime -> Text -> ConduitT () Choices (SqlPersistT (ResourceT IO)) ()
loadChoices loadSource now query =
  loadSource
    .| C.map (\(Value time, Value dir) -> (time, dir))
    .| CL.chunksOf 1024
    .| C.map (makeChoices now query)

refreshOptions :: Word8 -> Maybe (NonEmptyCursor Text) -> Choices -> Maybe (NonEmptyCursor Text)
refreshOptions maxOptions mOldOptions cs = do
  let newOptions =
        take ((fromIntegral :: Word8 -> Int) maxOptions)
          . map fst
          . sortOn (\(_, (fuzziness, score)) -> Ord.Down (fuzziness, score))
          $ M.toList $ choicesMap cs
  nec <- NE.nonEmpty newOptions
  pure $ case mOldOptions of
    -- If we didn't have any options before, just make a selection.
    Nothing -> makeNonEmptyCursor nec
    Just oldOptions -> do
      -- If we did have options before, try to reuse the same selection
      case makeNonEmptyCursorWithSelection (nonEmptyCursorSelection oldOptions) nec of
        Just nec' -> nec'
        -- If that doesn't work, just start over.
        Nothing -> makeNonEmptyCursor nec
