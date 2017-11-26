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
{-# LANGUAGE TemplateHaskell #-}
module TodoMVC.Component.TodoCount.Test.Harness (
  ) where

import Control.Lens

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT)
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
import Reflex.Test.Hedgehog

import TodoMVC.Common
import TodoMVC.Component.TodoCount
import TodoMVC.Component.TodoCount.Test

todoCountHarness ::
  MonadWidget t m =>
  m ()
todoCountHarness = do
  eClick <- buttonDynAttr (pure $ "id" =: "test-add") "Add"
  dSize <- count eClick
  todoCount dSize

clickTestAdd ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m Bool
clickTestAdd =
  checkMaybe $ clickButton =<< idElement "test-add"

data TestState (v :: * -> *) =
  TestState {
    _tsTodoCount :: TodoCountDOMState
  } deriving (Eq, Show)

makeLenses ''TestState

initialState ::
  TestState v
initialState =
  TestState initialTodoCountDOMState

instance HasTodoCountDOMState (TestState v) where
  todoCountDOMState = tsTodoCount

readTestState :: MaybeT TestJSM (TestState v)
readTestState =
  TestState <$> readTodoCountDOMState

data TestAdd (v :: * -> *) = TestAdd
  deriving (Eq, Show)

instance HTraversable TestAdd where
  htraverse _ TestAdd = pure TestAdd

s_add ::
  ( Monad n
  , MonadGen n
  , MonadTest m
  , MonadReader (TestingEnv (TestState Concrete)) m
  , MonadJSM m
  , HasDocument m
  ) =>
  Command n m TestState
s_add =
  let
    gen _ = Just $ pure TestAdd
    execute TestAdd = do
      clickTestAdd
      waitForRender
  in
    Command gen execute [
      Update $ \s TestAdd _o ->
        s & todoCountDOMState . tcText .~
            (if s ^. todoCountDOMState . tcCount == 0
             then " item left"
             else " items left")
          & tsTodoCount . tcCount +~ 1
    , Ensure $ \before after TestAdd b -> do
        after === b
        assert $ before ^. todoCountDOMState . tcCount + 1 == after ^. todoCountDOMState . tcCount
    ]

todoCountStateMachine ::
  PropertyT JSM ()
todoCountStateMachine = do
  env <- lift $ setupResettableWidgetWithHeadSection readTestState todoMVCHeadSection todoCountHarness
  runSequentialResettable env (Range.linear 1 100) initialState [
      s_add
    ]
