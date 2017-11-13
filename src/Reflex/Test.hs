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
module Reflex.Test (
    simulateClick
  , readOutput'
  , readOutput
  , TestingEnv
  , mkTestingEnv
  , mkTestingEnvWithHead
  , resetTest
  , waitForRender
  , waitForCondition
  , hoistCommand
  , ResettableState
  , initialResettableState
  , _Setup
  , _Running
  , s_reset
  , prismCommand
  , TestJSM(..)
  , propertyJSM
  ) where

import Control.Monad (void, unless)
import Data.Maybe (isJust)
import Text.Read (readMaybe)

import Data.Typeable (Typeable)

import Control.Lens

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
import Language.Javascript.JSaddle.Types (MonadJSM(..))

import Reflex.Dom.Core hiding (Command, Reset)
import Reflex.Dom (run)

import Hedgehog
import Hedgehog.Internal.Property

import Reflex.Helpers

simulateClick ::
  ( MonadReader (TestingEnv a) m
  , MonadJSM m
  ) =>
  Text ->
  m Bool
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
mkTestingEnv f b = do
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
      (testWidget b)
    pure doc

  liftIO . atomically . takeTMVar $ commitTMVar
  liftIO . atomically . readTQueue $ renderQueue

  pure $ TestingEnv renderQueue doc

mkTestingEnvWithHead ::
  (Document -> JSM a) ->
  Widget () () ->
  Widget () () ->
  JSM (TestingEnv a)
mkTestingEnvWithHead f h b = do
  commitTMVar <- liftIO . atomically $ newEmptyTMVar
  renderQueue <- liftIO . atomically $ newTQueue

  doc <- withJSContextSingletonMono $ \jsSing -> do
    doc <- currentDocumentUnchecked
    headElement <- getHeadUnchecked doc
    attachWidget headElement jsSing h
    body <- getBodyUnchecked doc
    attachWidgetWithActions
      (liftIO . atomically $ putTMVar commitTMVar ())
      (f doc >>= liftIO . atomically . writeTQueue renderQueue)
      body
      jsSing
      (testWidget b)
    pure doc

  liftIO . atomically . takeTMVar $ commitTMVar
  liftIO . atomically . readTQueue $ renderQueue

  pure $ TestingEnv renderQueue doc

resetTest ::
  ( MonadReader (TestingEnv a) m
  , MonadJSM m
  ) =>
  m a
resetTest = do
  simulateClick "reset-btn"
  waitForRender

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

hoistCommand ::
  (forall x. m x -> m' x) ->
  Command n m s ->
  Command n m' s
hoistCommand f (Command gen execute callbacks) =
  Command gen (f . execute) callbacks

prismCallback ::
  (forall v. Prism' (s v) (t v)) ->
  Callback i o t ->
  Callback i o s
prismCallback p (Require f) =
  Require $ \st i ->
    case preview p st of
      Just st' -> f st' i
      Nothing -> False
prismCallback p (Update f) =
  Update $ \st i v ->
    case preview p st of
      Just st' -> review p $ f st' i v
      Nothing -> st
prismCallback p (Ensure f) =
  Ensure $ \stb sta i o ->
    case (preview p sta, preview p stb) of
      (Just sta' , Just stb') -> f sta' stb' i o
      _ -> pure ()

prismCommand ::
  (forall v. Prism' (s v) (t v)) ->
  Command n m t ->
  Command n m s
prismCommand p (Command gen execute callbacks) =
  let
    gen' st = case preview p st of
      Just st' -> gen st'
      Nothing -> Nothing
    callbacks' = Require (\st _ -> isJust $ preview p st) : fmap (prismCallback p) callbacks
  in
    Command
      gen'
      execute
      callbacks'

data ResettableState st (v :: * -> *) =
    Setup
  | Running (st v)
  deriving (Eq, Ord, Show)

makePrisms ''ResettableState

initialResettableState :: ResettableState st v
initialResettableState = Setup

data Reset (v :: * -> *) = Reset
  deriving (Eq, Show)

instance HTraversable Reset where
  htraverse _ Reset = pure Reset

s_reset ::
  ( Monad n
  , MonadTest m
  , MonadReader (TestingEnv a) m
  , MonadJSM m
  , Typeable a
  , Show (st Concrete)
  , Eq (st Concrete)
  ) =>
  (forall v. st v) ->
  Command n m (ResettableState st)
s_reset initial =
  let
    gen st =
      case preview _Setup st of
        Just _ -> Just $ pure Reset
        _ -> Nothing
    execute Reset =
      resetTest
  in
    Command gen execute [
        Update $ \_s Reset _o ->
          review _Running initial
      , Ensure $ \_before after Reset _b ->
          review _Running initial === after
    ]

newtype TestJSM a =
  TestJSM { unTestJSM :: JSM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadJSM)

instance MonadJSM (TestT (GenT (ReaderT r TestJSM))) where
  liftJSM' = lift . lift . lift . TestJSM

instance MonadJSM (TestT (GenT TestJSM)) where
  liftJSM' = lift . lift . TestJSM

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
