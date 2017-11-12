{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Scratch where

import Control.Monad (void)
import Data.Foldable (forM_)
import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import Text.Read (readMaybe)

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Morph (hoist)

import Control.Monad.STM hiding (check)
import Control.Concurrent.STM hiding (check)

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Node
import GHCJS.DOM.Types (MonadJSM, JSM, askJSM, castTo)
import Language.Javascript.JSaddle.Monad (runJSaddle)

import Reflex.Dom.Core hiding (Element, Command, Reset)

import Reflex.Dom.Internal (run)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Internal.Property

buttonWithId ::
  MonadWidget t m =>
  Text ->
  Text ->
  m (Event t ())
buttonWithId eid label = do
  (e, _) <- elAttr' "button" ("id" =: eid) $ text label
  pure $ domEvent Click e

displayDivWithId ::
  ( MonadWidget t m
  , Show a
  ) =>
  Text ->
  Dynamic t a ->
  m ()
displayDivWithId eid d =
  elAttr "div" ("id" =: eid) $
    display d

simulateClick ::
  Text ->
  ReaderT (TestingEnv a) JSM Bool
simulateClick eid = do
  doc <- asks teDocument
  m <- runMaybeT $ do
    e  <- MaybeT . getElementById doc $ eid
    he <- MaybeT . castTo HTMLElement $ e
    lift . click $ he
  pure $ isJust m

readOutput' ::
  Read a =>
  proxy a ->
  Text ->
  Document ->
  JSM (Maybe a)
readOutput' _ =
  readOutput

readOutput ::
  Read a =>
  Text ->
  Document ->
  JSM (Maybe a)
readOutput eid doc = runMaybeT $ do
  e <- MaybeT . getElementById doc $ eid
  t <- MaybeT . getTextContent $ e
  MaybeT . pure . readMaybe . Text.unpack $ t

counter ::
  MonadWidget t m =>
  m ()
counter = do
  eAdd   <- buttonWithId "add-btn" "Add"
  eClear <- buttonWithId "clear-btn" "Clear"

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
              succ    <$ eAdd
            , const 0 <$ eClear
            ]

  displayDivWithId "count-output" dCount

  pure ()

testWidget ::
  MonadWidget t m =>
  m () ->
  m ()
testWidget w = do
  eReset <- buttonWithId "reset-btn" "Reset"
  _ <- widgetHold w (w <$ eReset)
  pure ()

data TestingEnv a =
  TestingEnv {
    teCommit :: TMVar ()
  , teQueue :: TQueue a
  , teDocument :: Document
  }

mkTestingEnv ::
  (Document -> JSM a) ->
  Widget () () ->
  JSM (TestingEnv a)
mkTestingEnv f w = do
  commitTMVar <- liftIO . atomically $ newEmptyTMVar
  renderQueue <- liftIO . atomically $ newTQueue

  doc <- withJSContextSingletonMono $ \jsSing -> do
    doc <- currentDocumentUnchecked
    body <- getBodyUnchecked doc
    attachWidgetWithActions
      (liftIO . atomically $ putTMVar commitTMVar ())
      (f doc >>= liftIO . atomically . writeTQueue renderQueue)
      body
      jsSing
      w
    pure doc

  liftIO . atomically . takeTMVar $ commitTMVar
  liftIO . atomically . readTQueue $ renderQueue

  pure $ TestingEnv commitTMVar renderQueue doc

waitForCommit ::
  ( MonadReader (TestingEnv a) m
  , MonadIO m
  ) =>
  m ()
waitForCommit = do
  v <- asks teCommit
  liftIO . atomically . takeTMVar $ v

waitForRender ::
  ( MonadReader (TestingEnv a) m
  , MonadIO m
  ) =>
  m a
waitForRender = do
  q <- asks teQueue
  liftIO . atomically . readTQueue $ q

data ModelState (v :: * -> *) =
    Setup
  | Running Int
  deriving (Eq, Ord, Show)

data Reset (v :: * -> *) = Reset
  deriving (Eq, Show)

instance HTraversable Reset where
  htraverse _ Reset = pure Reset

data Add (v :: * -> *) = Add
  deriving (Eq, Show)

instance HTraversable Add where
  htraverse _ Add = pure Add

data Clear (v :: * -> *) = Clear
  deriving (Eq, Show)

instance HTraversable Clear where
  htraverse _ Clear = pure Clear

s_reset ::
  ( Monad n
  ) =>
  TestingEnv (Maybe Int) ->
  Command n (TestT (GenT JSM)) ModelState
s_reset env =
  let
    gen st =
      case st of
        Setup -> Just $ pure Reset
        _ -> Nothing
    execute Reset = lift . lift . flip runReaderT env $ do
      _ <- simulateClick "reset-btn"
      waitForRender
  in
    Command gen execute [
        Update $ \_s Reset _o ->
            Running 0
      , Ensure $ \_before after Reset b -> do
          Running 0 === after
          Just 0 === b
    ]

s_add ::
  ( Monad n
  ) =>
  TestingEnv (Maybe Int) ->
  Command n (TestT (GenT JSM)) ModelState
s_add env =
  let
    gen st =
      case st of
        Running _ -> Just $ pure Add
        _ -> Nothing
    execute Add = lift . lift . flip runReaderT env $ do
      _ <- simulateClick "add-btn"
      waitForRender
  in
    Command gen execute [
        Require $ \s Add ->
            case s of
              Running _ -> True
              _ -> False
      , Update $ \s Add _o ->
          case s of
            Running c -> Running (c + 1)
            Setup -> Setup
      , Ensure $ \_before after Add b -> do
          case after of
            Running c -> do
              Just c === b
            _ -> pure ()
    ]

s_clear ::
  ( Monad n
  ) =>
  TestingEnv (Maybe Int) ->
  Command n (TestT (GenT JSM)) ModelState
s_clear env =
  let
    gen st =
      case st of
        Running _ -> Just $ pure Clear
        _ -> Nothing
    execute Clear = lift . lift . flip runReaderT env $ do
      _ <- simulateClick "clear-btn"
      waitForRender
  in
    Command gen execute [
        Require $ \s Clear ->
            case s of
              Running _ -> True
              _ -> False
      , Update $ \s Clear _o ->
          case s of
            Running _ -> Running 0
            Setup -> Setup
      , Ensure $ \_before after Clear b -> do
          case after of
            Running c -> do
              Just 0 === b
              0 === c
            _ -> pure ()
    ]

initialState ::
  ModelState v
initialState =
  Setup

counterStateMachine ::
  PropertyT JSM ()
counterStateMachine = do
  env <- lift $ mkTestingEnv (readOutput' (Proxy :: Proxy Int) "count-output") (testWidget counter)
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialState [
      s_reset env
    , s_add env
    , s_clear env
    ]
  PropertyT $ executeSequential initialState actions

counterProp :: JSM Bool
counterProp = do
  ctx <- askJSM
  liftIO .
    check .
    property .
    hoist (runJSaddle ctx) $
    counterStateMachine

doCounter ::
  IO ()
doCounter =
  run $ void counterProp 
