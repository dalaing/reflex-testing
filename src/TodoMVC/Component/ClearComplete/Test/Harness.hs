{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TodoMVC.Component.ClearComplete.Test.Harness (
  ) where

import Control.Monad (forM)
import Data.Maybe (isJust)

import Control.Lens

import Control.Monad.Trans (lift, MonadIO, liftIO)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad.Reader (MonadReader)

import Reflex.Dom.Core hiding (Command)
import Reflex.Dom (run)

import GHCJS.DOM.Types (MonadJSM, JSM)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Reflex.Test

import TodoMVC.Common
import TodoMVC.Component.ClearComplete
import TodoMVC.Component.ClearComplete.Test

clearCompleteHarness ::
  MonadWidget t m =>
  m ()
clearCompleteHarness = do
  eClick <- buttonDynAttr (pure $ "id" =: "test-toggle") "Toggle"
  dToggle <- toggle True eClick
  eRemove <- clearComplete dToggle
  dCount <- count eRemove
  elAttr "div" ("id" =: "test-count") $
    display dCount

clickTestToggle ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m Bool
clickTestToggle =
  simulateClick "test-toggle"

newtype TestCount = TestCount { getTestCount :: Int }
  deriving (Eq, Ord, Show, Read)

makeWrapped ''TestCount

class HasTestCount s where
  testCount :: Lens' s TestCount

instance HasTestCount TestCount where
  testCount = id

readTestCount :: MaybeT TestJSM TestCount
readTestCount = do
  i <- readOutput' "test-count"
  pure $ TestCount i

data TestState (v :: * -> *) =
  TestState {
    _tsClearComplete :: ClearCompleteDOMState
  , _tsTestCount :: TestCount
  } deriving (Eq, Show)

makeLenses ''TestState

initialState :: TestState v
initialState = TestState (ClearCompleteDOMState False) (TestCount 0)

instance HasClearCompleteDOMState (TestState v) where
  clearCompleteDOMState = tsClearComplete

instance HasTestCount (TestState v) where
  testCount = tsTestCount

readTestState :: MaybeT TestJSM (TestState v)
readTestState =
  TestState <$> readClearCompleteDOMState <*> readTestCount

data TestToggle (v :: * -> *) = TestToggle
  deriving (Eq, Show)

instance HTraversable TestToggle where
  htraverse _ TestToggle = pure TestToggle

s_toggle ::
  ( Monad n
  , MonadGen n
  , MonadTest m
  , MonadReader (TestingEnv (TestState Concrete)) m
  , MonadJSM m
  , HasDocument m
  ) =>
  Command n m TestState
s_toggle =
  let
    gen _ = Just $ pure TestToggle
    execute TestToggle = do
      clickTestToggle
      waitForRender
  in
    Command gen execute [
      Update $ \s TestToggle _o ->
          s & tsClearComplete . ccHidden %~ not
    , Ensure $ \before after TestToggle b -> do
        after === b
        assert $ before ^. tsClearComplete . ccHidden /= after ^. tsClearComplete . ccHidden
    ]

data ClickClearComplete (v :: * -> *) = ClickClearComplete
  deriving (Eq, Show)

instance HTraversable ClickClearComplete where
  htraverse _ ClickClearComplete = pure ClickClearComplete

s_clearComplete ::
  ( Monad n
  , MonadGen n
  , MonadTest m
  , MonadReader (TestingEnv (TestState Concrete)) m
  , MonadJSM m
  , HasDocument m
  ) =>
  Command n m TestState
s_clearComplete =
  let
    gen st =
      if st ^. tsClearComplete . ccHidden
      then Nothing
      else Just $ pure ClickClearComplete
    execute ClickClearComplete = do
      clickClearComplete
      waitForRender
  in
    Command gen execute [
      Require $ \s ClickClearComplete ->
        not $ s ^. tsClearComplete . ccHidden
    , Update $ \s ClickClearComplete _o ->
        s & tsTestCount . _Wrapped +~ 1
    , Ensure $ \before after ClickClearComplete b -> do
        after === b
        assert $ before ^. tsClearComplete == after ^. tsClearComplete
        assert $ before ^. tsTestCount . _Wrapped + 1 == after ^. tsTestCount . _Wrapped
    ]

data ClickClearCompleteHidden (v :: * -> *) = ClickClearCompleteHidden
  deriving (Eq, Show)

instance HTraversable ClickClearCompleteHidden where
  htraverse _ ClickClearCompleteHidden = pure ClickClearCompleteHidden

s_clearCompleteHidden ::
  ( Monad n
  , MonadGen n
  , MonadTest m
  , MonadReader (TestingEnv (TestState Concrete)) m
  , MonadJSM m
  , HasDocument m
  ) =>
  Command n m TestState
s_clearCompleteHidden =
  let
    gen st =
      if st ^. tsClearComplete . ccHidden
      then Just $ pure ClickClearCompleteHidden
      else Nothing
    execute ClickClearCompleteHidden = do
      clickClearComplete
      waitForRender
  in
    Command gen execute [
      Require $ \s ClickClearCompleteHidden ->
        s ^. tsClearComplete . ccHidden
    , Update $ \s ClickClearCompleteHidden _o ->
        s
    , Ensure $ \before after ClickClearCompleteHidden b -> do
        after === b
        before === after
    ]

clearCompleteStateMachine ::
  PropertyT JSM ()
clearCompleteStateMachine = do
  env <- lift $ setupResettableWidgetWithHeadSection readTestState todoMVCHeadSection clearCompleteHarness
  runSequentialResettable env (Range.linear 1 100) initialState [
      s_toggle
    , s_clearComplete
    , s_clearCompleteHidden
    ]
