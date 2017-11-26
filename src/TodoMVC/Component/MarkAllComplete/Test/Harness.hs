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
  eClick <- buttonDynAttr (pure $ "id" =: "test-toggle") "Toggle"
  dToggle <- foldDyn ($) False . leftmost $ [
               not <$ eClick
             , const <$> eComplete
             ]
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
  checkMaybe $ idElement "test-toggle" >>= clickButton

newtype TestComplete = TestComplete { getTestComplete :: Bool }
  deriving (Eq, Ord, Show, Read)

makeWrapped ''TestComplete

class HasTestComplete s where
  testComplete :: Lens' s TestComplete

instance HasTestComplete TestComplete where
  testComplete = id

readTestComplete :: MaybeT TestJSM TestComplete
readTestComplete = do
  b <- idElement "test-complete" >>= readText'
  pure $ TestComplete b

data TestState (v :: * -> *) =
  TestState {
    _tsMarkAllComplete :: MarkAllCompleteDOMState
  , _tsTestComplete :: TestComplete
  } deriving (Eq, Show)

makeLenses ''TestState

initialState :: TestState v
initialState = TestState initialMarkAllCompleteDOMState (TestComplete False)

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
          let old = s ^. testComplete . _Wrapped
              new = not old
          in
            s & testComplete . _Wrapped .~ new
              & markAllCompleteDOMState . macChecked .~ new
    , Ensure $ \before after TestToggle b -> do
        after === b
        assert $ before ^. testComplete /= after ^. testComplete
        assert $ after ^. markAllCompleteDOMState . macChecked == after ^. testComplete . _Wrapped
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
        s & markAllCompleteDOMState . macChecked %~ not
    , Ensure $ \before after ToggleMarkAllComplete b -> do
        after === b
        assert $ after ^. markAllCompleteDOMState . macChecked == after ^. testComplete . _Wrapped
    ]

markAllCompleteStateMachine ::
  PropertyT JSM ()
markAllCompleteStateMachine = do
  env <- lift $ setupResettableWidgetWithHeadSection readTestState todoMVCHeadSection markAllCompleteHarness
  runSequentialResettable env (Range.linear 1 100) initialState [
      s_toggle
    , s_markAllComplete
    ]
