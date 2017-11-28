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
module TodoMVC.Component.TodoList.TodoItem.Read.Remove.Test.Harness (
    removeStateMachine
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
import Reflex.Test.Id
import Reflex.Test.Text
import Reflex.Test.Hedgehog

import TodoMVC.Common
import TodoMVC.Component.TodoList.TodoItem.Read.Remove
import TodoMVC.Component.TodoList.TodoItem.Read.Remove.Test

removeHarness ::
  MonadWidget t m =>
  m ()
removeHarness = do
  eRemove <- remove
  dCount <- count eRemove
  elAttr "div" ("id" =: "remove-count") $
    display dCount

newtype TestCount = TestCount { getTestCount :: Int }
  deriving (Eq, Ord, Show, Read)

makeWrapped ''TestCount

class HasTestCount s where
  testCount :: Lens' s TestCount

instance HasTestCount TestCount where
  testCount = id

readTestCount :: MaybeT TestJSM TestCount
readTestCount = do
  i <- idElement "remove-count" >>= readText'
  pure $ TestCount i

data TestState (v :: * -> *) =
  TestState {
    _tsTestCount :: TestCount
  } deriving (Eq, Show)

makeLenses ''TestState

initialState :: TestState v
initialState = TestState (TestCount 0)

instance HasTestCount (TestState v) where
  testCount = tsTestCount

readTestState :: MaybeT TestJSM (TestState v)
readTestState =
  TestState <$> readTestCount

data ClickRemove (v :: * -> *) = ClickRemove
  deriving (Eq, Show)

instance HTraversable ClickRemove where
  htraverse _ ClickRemove = pure ClickRemove

s_remove ::
  ( Monad n
  , MonadGen n
  , MonadTest m
  , MonadReader (TestingEnv (TestState Concrete)) m
  , MonadJSM m
  , HasDocument m
  ) =>
  Command n m TestState
s_remove =
  let
    gen _ = Just $ pure ClickRemove
    execute ClickRemove = do
      clickRemove
      waitForRender
  in
    Command gen execute [
      Update $ \s ClickRemove _o ->
        s & tsTestCount . _Wrapped +~ 1
    , Ensure $ \before after ClickRemove b -> do
        after === b
        assert $ before ^. tsTestCount . _Wrapped + 1 == after ^. tsTestCount . _Wrapped
    ]

removeStateMachine ::
  PropertyT JSM ()
removeStateMachine = do
  env <- lift $ setupResettableWidgetWithHeadSection readTestState todoMVCHeadSection removeHarness
  runSequentialResettable env (Range.linear 1 100) initialState [
      s_remove
    ]
