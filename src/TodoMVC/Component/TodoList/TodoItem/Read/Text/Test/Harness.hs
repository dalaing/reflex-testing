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
module TodoMVC.Component.TodoList.TodoItem.Read.Text.Test.Harness (
  ) where

import Control.Lens

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad.Reader (MonadReader)

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core hiding (Command)
import GHCJS.DOM.Types (MonadJSM, JSM)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Reflex.Test
import Reflex.Test.Maybe
import Reflex.Test.Class
import Reflex.Test.Id
import Reflex.Test.Tag
import Reflex.Test.Text
import Reflex.Test.Button
import Reflex.Test.Hedgehog

import TodoMVC.Common
import TodoMVC.Component.TodoList.TodoItem.Read.Text
import TodoMVC.Component.TodoList.TodoItem.Read.Text.Test

textReadHarness ::
  MonadWidget t m =>
  m ()
textReadHarness = do
  eClick <- buttonDynAttr (pure $ "id" =: "test-toggle-btn") "Toggle"
  dToggle <- toggle False eClick
  eClick <- elAttr "div" ("id" =: "test-out") $
    textRead $ (Text.pack . show) <$> dToggle
  dCount <- count eClick
  elAttr "div" ("id" =: "test-count") $
    display dCount

clickToggle ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m Bool
clickToggle =
  checkMaybe $ idElement "test-toggle-btn" >>= clickButton

dblclickText ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m Bool
dblclickText =
  checkMaybe $ idElement "test-out" >>= \e -> tagElementsSingle (Just e) "label" >>= dblclick

newtype TestCount = TestCount { getTestCount :: Int }
  deriving (Eq, Ord, Show, Read)

makeWrapped ''TestCount

class HasTestCount s where
  testCount :: Lens' s TestCount

instance HasTestCount TestCount where
  testCount = id

readTestCount :: MaybeT TestJSM TestCount
readTestCount = do
  i <- idElement "test-count" >>= readText'
  pure $ TestCount i

readTestText :: MaybeT TestJSM Text
readTestText =
  idElement "test-out" >>= \e -> tagElementsSingle (Just e) "label" >>= readText'

data TestState (v :: * -> *) =
  TestState {
    _tsTestText :: Text
  , _tsTestCount :: TestCount
  } deriving (Eq, Show)

makeLenses ''TestState

initialState :: TestState v
initialState = TestState "False" (TestCount 0)

instance HasTestCount (TestState v) where
  testCount = tsTestCount

readTestState :: MaybeT TestJSM (TestState v)
readTestState =
  TestState <$> readTestText <*> readTestCount

data ClickToggle (v :: * -> *) = ClickToggle
  deriving (Eq, Show)

instance HTraversable ClickToggle where
  htraverse _ ClickToggle = pure ClickToggle

s_clickToggle ::
  ( Monad n
  , MonadGen n
  , MonadTest m
  , MonadReader (TestingEnv (TestState Concrete)) m
  , MonadJSM m
  , HasDocument m
  ) =>
  Command n m TestState
s_clickToggle =
  let
    gen _ = Just $ pure ClickToggle
    execute ClickToggle = do
      clickToggle
      waitForRender
  in
    Command gen execute [
      Update $ \s ClickToggle _o ->
        s & tsTestText %~ Text.pack . show . not . read . Text.unpack
    , Ensure $ \before after ClickToggle b -> do
        after === b
        assert $ before ^. tsTestText /= after ^. tsTestText
        assert $ before ^. tsTestCount == after ^. tsTestCount
    ]

data DblclickText (v :: * -> *) = DblclickText
  deriving (Eq, Show)

instance HTraversable DblclickText where
  htraverse _ DblclickText = pure DblclickText

s_dblClickText ::
  ( Monad n
  , MonadGen n
  , MonadTest m
  , MonadReader (TestingEnv (TestState Concrete)) m
  , MonadJSM m
  , HasDocument m
  ) =>
  Command n m TestState
s_dblClickText =
  let
    gen _ = Just $ pure DblclickText
    execute DblclickText = do
      dblclickText
      waitForRender
  in
    Command gen execute [
      Update $ \s DblclickText _o ->
        s & tsTestCount . _Wrapped +~ 1
    , Ensure $ \before after DblclickText b -> do
        after === b
        assert $ before ^. tsTestText == after ^. tsTestText
        assert $ before ^. tsTestCount . _Wrapped + 1 == after ^. tsTestCount . _Wrapped
    ]

textReadStateMachine ::
  PropertyT JSM ()
textReadStateMachine = do
  env <- lift $ setupResettableWidgetWithHeadSection readTestState todoMVCHeadSection textReadHarness
  runSequentialResettable env (Range.linear 1 100) initialState [
      s_clickToggle
    , s_dblClickText
    ]
