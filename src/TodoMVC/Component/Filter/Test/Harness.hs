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
{-# LANGUAGE KindSignatures #-}
module TodoMVC.Component.Filter.Test.Harness (
    filterStateMachine
  ) where

import Control.Lens

import qualified Data.Set as Set

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Reader (MonadReader)

import Reflex.Dom.Core hiding (Command)

import GHCJS.DOM.Types (MonadJSM, JSM)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Reflex.Test

import TodoMVC.Common
import TodoMVC.Component.Filter
import TodoMVC.Component.Filter.Test

filterHarness ::
  MonadWidget t m =>
  m ()
filterHarness = do
  dFilter <- filtersWidget
  elAttr "div" ("id" =: "test-filter") $
    display dFilter

newtype TestFilter = TestFilter { getTestFilter :: Filter }
  deriving (Eq, Ord, Show, Read)

makeWrapped ''TestFilter

class HasTestFilter s where
  testFilter :: Lens' s TestFilter

instance HasTestFilter TestFilter where
  testFilter = id

readTestFilter :: MaybeT TestJSM TestFilter
readTestFilter = do
  f <- readOutput' "test-filter"
  pure $ TestFilter f

data TestState (v :: * -> *) =
  TestState {
    _tsFilters :: FiltersDOMState
  , _tsTestFilter :: TestFilter
  } deriving (Eq, Show)

makeLenses ''TestState

initialState :: TestState v
initialState = TestState initialFiltersDOMState (TestFilter FAll)

instance HasFiltersDOMState (TestState v) where
  filtersDOMState = tsFilters

instance HasTestFilter (TestState v) where
  testFilter = tsTestFilter

readTestState :: MaybeT TestJSM (TestState v)
readTestState =
  TestState <$> readFiltersDOMState <*> readTestFilter

data ClickFilter (v :: * -> *) = ClickFilter Filter
  deriving (Eq, Show)

instance HTraversable ClickFilter where
  htraverse _ (ClickFilter cf) = pure (ClickFilter cf)

s_filter ::
  ( Monad n
  , MonadGen n
  , MonadTest m
  , MonadReader (TestingEnv (TestState Concrete)) m
  , MonadJSM m
  , HasDocument m
  ) =>
  Command n m TestState
s_filter =
  let
    gen _ =
      Just $ ClickFilter <$> Gen.element [FAll, FActive, FComplete]
    execute (ClickFilter f) = do
      clickFilter f
      waitForRender
  in
    Command gen execute [
      Update $ \s (ClickFilter f) _o ->
          s & filtersDOMState . fSelected .~ Set.singleton f
            & tsTestFilter . _Wrapped .~ f
    , Ensure $ \before after (ClickFilter f) b -> do
        after === b
        after ^. filtersDOMState . fSelected === Set.singleton f
        after ^. tsTestFilter . _Wrapped === f
    ]

filterStateMachine ::
  PropertyT JSM ()
filterStateMachine = do
  env <- lift $ setupResettableWidgetWithHeadSection readTestState todoMVCHeadSection filterHarness
  runSequentialResettable env (Range.linear 1 100) initialState [
      s_filter
    ]
