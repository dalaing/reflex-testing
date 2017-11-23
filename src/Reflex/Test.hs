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
    TestJSM(..)
  , TestingEnv(..)
  , mkTestingEnv
  , runTestingEnv
  , liftStateHook
  , testingWidget
  , setupResettableWidget
  , setupResettableWidgetWithHeadSection
  , resetTest
  , waitForRender
  , waitForCondition

  , classElementsSingle
  , classElementsMultiple
  , classElementsIx
  , simulateClick
  , readOutput
  , readOutput'

  , hoistCommand
  , hoistAction
  , hoistSequential
  , sequentialResettable
  , runSequentialResettable
  , ResettableState
  , initialResettableState
  , _Setup
  , _Running
  , s_reset
  , prismCommand
  , propertyJSM
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

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Internal.Property
import Hedgehog.Internal.State

import Reflex.Helpers

newtype TestJSM a =
  TestJSM { unTestJSM :: JSM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadJSM)

instance MonadJSM (TestT (GenT (ReaderT r TestJSM))) where
  liftJSM' = lift . lift . lift . liftJSM'

instance MonadJSM (TestT (GenT TestJSM)) where
  liftJSM' = lift . lift . liftJSM'

instance HasDocument TestJSM where
  askDocument = TestJSM currentDocumentUnchecked

instance HasDocument (TestT (GenT TestJSM)) where
  askDocument = lift $ lift $ askDocument

instance HasDocument (TestT (GenT (ReaderT r TestJSM))) where
  askDocument = lift $ lift $ lift $ askDocument

data TestingEnv a =
  TestingEnv {
    _teQueue    :: TMVar a
  }

makeClassy ''TestingEnv

mkTestingEnv :: STM (TestingEnv a)
mkTestingEnv =
  TestingEnv <$> newEmptyTMVar

runTestingEnv ::
  TestingEnv e ->
  TestT (GenT (ReaderT (TestingEnv e) TestJSM)) a ->
  TestT (GenT JSM) a
runTestingEnv env =
  hoist $ hoist $ unTestJSM . flip runReaderT env

classElements ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Text ->
  m (Word, HTMLCollection)
classElements eclass = do
  doc <- askDocument
  c <- getElementsByClassName doc eclass
  l <- getLength c
  pure (l, c)

classElementsSingle ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Text ->
  (Element -> MaybeT m a) ->
  MaybeT m a
classElementsSingle eclass f = do
  doc <- lift askDocument
  (l, c) <- lift $ classElements eclass
  if (l /= 1)
  then MaybeT . pure $ Nothing
  else do
    e <- MaybeT $ item c 0
    f e

classElementsMultiple ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Text ->
  (Element -> MaybeT m a) ->
  MaybeT m [a]
classElementsMultiple eclass f = do
  (l, c) <- lift $ classElements eclass
  if (l == 0)
  then pure []
  else forM [0..l-1] $ \i -> do
    e <- MaybeT $ item c i
    f e

classElementsIx ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Word ->
  Text ->
  (Element -> MaybeT m a) ->
  MaybeT m a
classElementsIx i eclass f = do
  (l, c) <- lift $ classElements eclass
  if (i < 0 || l <= i)
  then MaybeT . pure $ Nothing
  else do
    e <- MaybeT $ item c i
    f e

idHtmlElement ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Text ->
  MaybeT m HTMLElement
idHtmlElement eid = do
  doc <- lift askDocument
  e  <- MaybeT . getElementById doc $ eid
  MaybeT . castTo HTMLElement $ e

simulateClick ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Text ->
  m Bool
simulateClick eid = do
  m <- runMaybeT $ do
    he <- idHtmlElement eid
    lift . click $ he
  pure $ isJust m

readOutput ::
  ( Read a
  , MonadJSM m
  , HasDocument m
  ) =>
  proxy a ->
  Text ->
  MaybeT m a
readOutput _ =
  readOutput'

readOutput' ::
  ( Read a
  , MonadJSM m
  , HasDocument m
  ) =>
  Text ->
  MaybeT m a
readOutput' eid = do
  doc <- lift askDocument
  e <- MaybeT . getElementById doc $ eid
  t <- MaybeT . getTextContent $ e
  MaybeT . pure . readMaybe . Text.unpack $ t

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
  void $ simulateClick "reset-btn"
  waitForRender

setResultDone ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m ()
setResultDone = do
  simulateClick "result-stop-btn"
  pure ()

clearResultDone ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m ()
clearResultDone = do
  simulateClick "result-start-btn"
  pure ()

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

hoistCommand ::
  (forall x. m x -> m' x) ->
  Command n m s ->
  Command n m' s
hoistCommand f (Command gen execute callbacks) =
  Command gen (f . execute) callbacks

hoistAction ::
  (forall x. m x -> m' x) ->
  Action m s ->
  Action m' s
hoistAction f (Action aInput aOutput aExecute aRequire aUpdate aEnsure) =
  Action aInput aOutput (f . aExecute) aRequire aUpdate aEnsure

hoistSequential ::
  (forall x. m x -> m' x) ->
  Sequential m s ->
  Sequential m' s
hoistSequential f (Sequential xs) =
  Sequential (fmap (hoistAction f) xs)

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
    case (preview p stb, preview p sta) of
      (Just stb' , Just sta') -> f stb' sta' i o
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
  , HasDocument m
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
        Require $ \s Reset ->
            isJust (preview _Setup s)
      , Update $ \_s Reset _o ->
          review _Running initial
      , Ensure $ \_before after Reset _b ->
          review _Running initial === after
    ]

sequentialResettable ::
  ( MonadGen n
  , MonadTest m
  , MonadReader (TestingEnv e) m
  , MonadJSM m
  , HasDocument m
  , Typeable e
  , Eq (state Concrete)
  , Show (state Concrete)
  ) =>
  Range.Range Int ->
  (forall v. state v) ->
  [Command n m state] ->
  n (Sequential m (ResettableState state))
sequentialResettable range initial commands = do
  Gen.sequential range initialResettableState $
    s_reset initial : fmap (prismCommand _Running) commands

runSequentialResettable ::
  ( Typeable e
  , Eq (state Concrete)
  , Show (state Concrete)
  ) =>
  TestingEnv e ->
  Range.Range Int ->
  (forall v. state v) ->
  [Command (GenT Identity) (TestT (GenT (ReaderT (TestingEnv e) TestJSM))) state] ->
  PropertyT JSM ()
runSequentialResettable env range initial commands = do
  actions <- forAll $ do
    s <- sequentialResettable range initial commands
    pure $ hoistSequential (runTestingEnv env) s
  PropertyT $ executeSequential initialResettableState actions

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

