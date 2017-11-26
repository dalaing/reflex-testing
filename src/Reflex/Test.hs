{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Reflex.Test (
    liftStateHook
  , testingWidget
  , setupResettableWidget
  , setupResettableWidgetWithHeadSection
  , resetTest
  , waitForRender
  , waitForCondition
  , module Reflex.Test.Types
  ) where

import Control.Monad (void, unless, when, forM, forM_)
import Data.Maybe (isJust, fromMaybe)
import Text.Read (readMaybe)

import Data.Typeable (Typeable)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Monad.STM hiding (check)
import Control.Concurrent.STM hiding (check)

import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Trans.Maybe
import Control.Monad.Morph (hoist)

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.Element hiding (getElementsByClassName)
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.HTMLCollection
import GHCJS.DOM.Node
import GHCJS.DOM.Types (MonadJSM, JSM, askJSM, castTo)
import Language.Javascript.JSaddle.Monad (runJSaddle)
import Language.Javascript.JSaddle.Types (MonadJSM(..), liftJSM)

import Reflex.Dom.Core hiding (Command, Reset, Element)
import Reflex.Dom (run)

import Reflex.Helpers

import Reflex.Test.Types
import Reflex.Test.Maybe
import Reflex.Test.Id
import Reflex.Test.Button

testWidget ::
  MonadWidget t m =>
  m () ->
  m ()
testWidget w = do
  eReset <- buttonWithId "reset-btn" "Reset"

  eStart <- buttonWithId "result-start-btn" "Start"
  eStop <- buttonWithId "result-stop-btn" "Stop"

  dResult <- holdDyn "" . leftmost $
    [ "" <$ eStart
    , "Done" <$ eStop
    ]

  elAttr "text" ("id" =: "result-text") $ do
    dynText dResult

  _ <- widgetHold w (w <$ eReset)
  pure ()

resetTest ::
  ( MonadReader (TestingEnv a) m
  , MonadJSM m
  , HasDocument m
  ) =>
  m a
resetTest = do
  void . checkMaybe $
    clickButton =<< idElement "reset-btn"
  waitForRender

setResultDone ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m ()
setResultDone =
  void . checkMaybe $
    clickButton =<< idElement "result-stop-btn"

clearResultDone ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m ()
clearResultDone =
  void . checkMaybe $
    clickButton =<< idElement "result-start-btn"

getResultDone ::
  Document ->
  JSM Bool
getResultDone doc = do
  mt <- runMaybeT $ do
    e <- MaybeT $ getElementById doc ("result-text" :: Text)
    t <- MaybeT . getTextContent $ e
    pure $ t == ("Done" :: Text)

  pure $ fromMaybe False mt

liftStateHook ::
  Monad m =>
  StateT s m Bool ->
  s ->
  MaybeT m s
liftStateHook m s = do
  (b, s') <- lift $ runStateT m s
  if b
  then MaybeT $ pure (Just s')
  else MaybeT $ pure Nothing

testingEnvHook ::
  (MaybeT TestJSM a) ->
  TestingEnv a ->
  JSM x ->
  JSM x
testingEnvHook f (TestingEnv tqRender) h = do
  x <- h

  mDoc <- currentDocument
  forM_ mDoc $ \doc -> do
    done <- getResultDone doc
    when done $ do
      ma <- unTestJSM . runMaybeT $ f
      forM_ ma $
        liftIO . atomically . putTMVar tqRender

  pure x

testingWidget ::
  ( MonadWidget t m
  , DomRenderHook t m
  ) =>
  (MaybeT TestJSM a) ->
  TestingEnv a ->
  m () ->
  m ()
testingWidget f e =
  withRenderHook (testingEnvHook f e) .
  testWidget

setupResettableWidget ::
  ( MonadJSM n
  , MonadIO n
  ) =>
  (MaybeT TestJSM a) ->
  (forall t m. (MonadWidget t m, DomRenderHook t m) => m ()) ->
  n (TestingEnv a)
setupResettableWidget f w = do
  env <- liftIO . atomically $ mkTestingEnv
  liftJSM $ do
    mainWidget $ testingWidget f env $ w
    unTestJSM . runReaderT resetTest $ env
  pure env

setupResettableWidgetWithHeadSection ::
  ( MonadJSM n
  , MonadIO n
  ) =>
  (MaybeT TestJSM a) ->
  (forall t m. (MonadWidget t m, DomRenderHook t m) => m ()) ->
  (forall t m. (MonadWidget t m, DomRenderHook t m) => m ()) ->
  n (TestingEnv a)
setupResettableWidgetWithHeadSection f h w = do
  env <- liftIO . atomically $ mkTestingEnv
  liftJSM $ do
    mainWidgetWithHead h $ testingWidget f env $ w
    unTestJSM . runReaderT resetTest $ env
  pure env

waitForRender ::
  ( MonadReader (TestingEnv a) m
  , MonadIO m
  , MonadJSM m
  , HasDocument m
  ) =>
  m a
waitForRender = do
  setResultDone
  q <- view teQueue
  a <- liftIO . atomically . takeTMVar $ q
  clearResultDone
  pure a

waitForCondition ::
  ( MonadReader (TestingEnv a) m
  , MonadIO m
  , MonadJSM m
  , HasDocument m
  ) =>
  (a -> Bool) ->
  m ()
waitForCondition p = do
  x <- waitForRender
  unless (p x) $
    waitForCondition p

