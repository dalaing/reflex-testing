{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Test (
    simulateClick
  , readOutput'
  , readOutput
  , testWidget
  , TestingEnv
  , mkTestingEnv
  , waitForRender
  , waitForCondition
  , propertyJSM
  ) where

import Control.Monad (void, unless)
import Data.Maybe (isJust)
import Text.Read (readMaybe)

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Monad.STM hiding (check)
import Control.Concurrent.STM hiding (check)

import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans.Maybe
import Control.Monad.Morph (hoist)

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Node
import GHCJS.DOM.Types (MonadJSM, JSM, askJSM, castTo)
import Language.Javascript.JSaddle.Monad (runJSaddle)

import Reflex.Dom.Core
import Reflex.Dom (run)

import Hedgehog
import Hedgehog.Internal.Property

import Reflex.Helpers

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
    teQueue :: TQueue a
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

  pure $ TestingEnv renderQueue doc

waitForRender ::
  ( MonadReader (TestingEnv a) m
  , MonadIO m
  ) =>
  m a
waitForRender = do
  q <- asks teQueue
  liftIO . atomically . readTQueue $ q

waitForCondition ::
  ( MonadReader (TestingEnv a) m
  , MonadIO m
  ) =>
  (a -> Bool) ->
  m ()
waitForCondition p = do
  x <- waitForRender
  unless (p x) $
    waitForCondition p

propertyJSM ::
  PropertyT JSM () ->
  IO ()
propertyJSM p = run . void $ do
  ctx <- askJSM
  liftIO .
    check .
    property .
    hoist (runJSaddle ctx) $
    p
