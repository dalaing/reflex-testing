{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TodoMVC.Component.MarkAllComplete.Test.Harness (
    markAllCompleteStateMachine
  ) where

import Control.Lens

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad.Reader (MonadReader)

import Reflex.Dom.Core hiding (Command)

import GHCJS.DOM.Types (MonadJSM, JSM)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Reflex.Test
import Reflex.Test.Maybe
import Reflex.Test.Id
import Reflex.Test.Button
import Reflex.Test.Text
import Reflex.Test.Hedgehog

import TodoMVC.Common
import TodoMVC.Component.MarkAllComplete
import TodoMVC.Component.MarkAllComplete.Test

markAllCompleteHarness ::
  MonadWidget t m =>
  m ()
markAllCompleteHarness = mdo
  eClick <- buttonDynAttr (pure $ "id" =: "test-toggle-btn") "Toggle"
  dToggle <- toggle False eClick
  elAttr "div" ("id" =: "test-toggle") $
    display dToggle

  eComplete <- markAllComplete False dToggle
  dComplete <- holdDyn False eComplete
  elAttr "div" ("id" =: "test-complete") $
    display dComplete

  pure ()

clickTestToggle ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m Bool
clickTestToggle =
  checkMaybe $ idElement "test-toggle-btn" >>= clickButton

data TestComplete = TestComplete {
    _tcToggle :: Bool
  , _tcComplete :: Bool
  }
  deriving (Eq, Ord, Show, Read)

makeClassy ''TestComplete

readTestComplete :: MaybeT TestJSM TestComplete
readTestComplete = do
  bt <- idElement "test-toggle" >>= readText'
  bc <- idElement "test-complete" >>= readText'
  pure $ TestComplete bt bc

data TestState (v :: * -> *) =
  TestState {
    _tsMarkAllComplete :: MarkAllCompleteDOMState
  , _tsTestComplete :: TestComplete
  } deriving (Eq, Show)

makeLenses ''TestState

initialState :: TestState v
initialState = TestState initialMarkAllCompleteDOMState (TestComplete False False)

instance HasMarkAllCompleteDOMState (TestState v) where
  markAllCompleteDOMState = tsMarkAllComplete

instance HasTestComplete (TestState v) where
  testComplete = tsTestComplete

readTestState :: MaybeT TestJSM (TestState v)
readTestState =
  TestState <$> readMarkAllCompleteDOMState <*> readTestComplete

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
          let old = s ^. tcToggle
              new = not old
          in
            s & tcToggle .~ new
              & macChecked .~ new
    , Ensure $ \before after TestToggle b -> do
        after === b
        assert $ before ^. tcToggle /= after ^. tcToggle
        assert $ before ^. tcComplete == after ^. tcComplete
        assert $ after ^. macChecked == after ^. tcToggle
    ]

data ToggleMarkAllComplete (v :: * -> *) = ToggleMarkAllComplete
  deriving (Eq, Show)

instance HTraversable ToggleMarkAllComplete where
  htraverse _ ToggleMarkAllComplete = pure ToggleMarkAllComplete

s_markAllComplete ::
  ( Monad n
  , MonadGen n
  , MonadTest m
  , MonadReader (TestingEnv (TestState Concrete)) m
  , MonadJSM m
  , HasDocument m
  ) =>
  Command n m TestState
s_markAllComplete =
  let
    gen _ = Just . pure $ ToggleMarkAllComplete
    execute ToggleMarkAllComplete = do
      toggleMarkAllComplete
      waitForRender
  in
    Command gen execute [
      Update $ \s ToggleMarkAllComplete _o ->
        let
          old = s ^. macChecked
          new = not old
        in
          s & markAllCompleteDOMState . macChecked .~ new
            & tcComplete .~ new
    , Ensure $ \before after ToggleMarkAllComplete b -> do
        after === b
        assert $ before ^. tcToggle == after ^. tcToggle
        assert $ before ^. macChecked /= after ^. macChecked
        assert $ after ^. macChecked == after ^. tcComplete
    ]

markAllCompleteStateMachine ::
  PropertyT JSM ()
markAllCompleteStateMachine = do
  env <- lift $ setupResettableWidgetWithHeadSection readTestState todoMVCHeadSection markAllCompleteHarness
  runSequentialResettable env (Range.linear 1 100) initialState [
      s_toggle
    , s_markAllComplete
    ]
