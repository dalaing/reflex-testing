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
    TestingEnv(..)
  , mkTestingEnv
  , GetDocument(..)
  , getDocument
  , testingWidget
  , resetTest
  , waitForRender
  , waitForCondition

  , classElementsSingle
  , classElementsMultiple
  , classElementsIx
  , simulateClick
  , readOutput'
  , readOutput

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
import Language.Javascript.JSaddle.Types (MonadJSM(..))

import Reflex.Dom.Core hiding (Command, Reset, Element)
import Reflex.Dom (run)

import Hedgehog
import Hedgehog.Internal.Property

import Reflex.Helpers

data TestingEnv a =
  TestingEnv {
    _teDocument :: TMVar Document
  , _teQueue    :: TQueue a
  }

makeClassy ''TestingEnv

mkTestingEnv :: STM (TestingEnv a)
mkTestingEnv =
  TestingEnv <$> newEmptyTMVar <*> newTQueue

class GetDocument d where
  getDocument' :: MonadIO m => d -> m Document

instance GetDocument Document where
  getDocument' = pure

instance GetDocument (TestingEnv a) where
  getDocument' =
    liftIO .
    atomically .
    readTMVar .
    _teDocument

getDocument ::
  ( GetDocument d
  , MonadReader d m
  , MonadIO m
  ) =>
  m Document
getDocument =
  ask >>= getDocument'

classElements ::
  ( GetDocument d
  , MonadReader d m
  , MonadJSM m
  ) =>
  Text ->
  m (Word, HTMLCollection)
classElements eclass = do
  doc <- getDocument
  c <- getElementsByClassName doc eclass
  l <- getLength c
  pure (l, c)

classElementsSingle ::
  ( GetDocument d
  , MonadReader d m
  , MonadJSM m
  ) =>
  Text ->
  (Element -> MaybeT m a) ->
  MaybeT m a
classElementsSingle eclass f = do
  doc <- getDocument
  (l, c) <- lift $ classElements eclass
  if (l /= 1)
  then MaybeT . pure $ Nothing
  else do
    e <- MaybeT $ item c 0
    f e

classElementsMultiple ::
  ( GetDocument d
  , MonadReader d m
  , MonadJSM m
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
  ( GetDocument d
  , MonadReader d m
  , MonadJSM m
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
  ( GetDocument d
  , MonadReader d m
  , MonadJSM m
  ) =>
  Text ->
  MaybeT m HTMLElement
idHtmlElement eid = do
  doc <- getDocument
  e  <- MaybeT . getElementById doc $ eid
  MaybeT . castTo HTMLElement $ e

simulateClick ::
  ( GetDocument d
  , MonadReader d m
  , MonadJSM m
  ) =>
  Text ->
  m Bool
simulateClick eid = do
  m <- runMaybeT $ do
    he <- idHtmlElement eid
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

  elAttr "text" ("id" =: "result-text") $ do
    dResult <- holdDyn "" ("" <$ eReset)
    dynText dResult

  _ <- widgetHold w (w <$ eReset)
  pure ()

resetTest ::
  ( MonadReader (TestingEnv a) m
  , MonadJSM m
  ) =>
  m a
resetTest = do
  simulateClick "reset-btn"
  waitForRender

setResultDone ::
  ( GetDocument d
  , MonadReader d m
  , MonadJSM m
  ) =>
  m ()
setResultDone = do
  doc <- getDocument
  _ <- runMaybeT $ do
    e <- MaybeT $ getElementById doc ("result-text" :: Text)
    lift . setTextContent e . Just $ ("Done" :: Text)
  pure ()

clearResultDone ::
  Document ->
  JSM ()
clearResultDone doc = do
  _ <- runMaybeT $ do
    e <- MaybeT $ getElementById doc ("result-text" :: Text)
    lift . setTextContent e . Just $ ("" :: Text)
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

testingEnvHook ::
  -- (Document -> JSM (Maybe a)) ->
  (Document -> JSM a) ->
  TestingEnv a ->
  JSM x ->
  JSM x
testingEnvHook f (TestingEnv tmDoc tqRender) h = do
  x <- h

  mDoc <- currentDocument
  forM_ mDoc $ \doc -> do
    liftIO . atomically . tryPutTMVar tmDoc $ doc

    done <- getResultDone doc
    when done $ do
      clearResultDone doc
      a <- f doc
      liftIO . atomically . writeTQueue tqRender $ a

    pure ()


  pure x

testingWidget ::
  ( MonadWidget t m
  , DomRenderHook t m
  ) =>
  (Document -> JSM a) ->
  TestingEnv a ->
  m () ->
  m ()
testingWidget f e =
  withRenderHook (testingEnvHook f e) .
  testWidget

waitForRender ::
  ( MonadReader (TestingEnv a) m
  , MonadIO m
  , MonadJSM m
  ) =>
  m a
waitForRender = do
  setResultDone
  q <- view teQueue
  liftIO . atomically . readTQueue $ q

waitForCondition ::
  ( MonadReader (TestingEnv a) m
  , MonadIO m
  , MonadJSM m
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

