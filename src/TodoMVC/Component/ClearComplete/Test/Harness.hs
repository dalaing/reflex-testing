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
{-# LANGUAGE MultiParamTypeClasses #-}
module TodoMVC.Component.ClearComplete.Test.Harness (
  ) where

import Control.Monad (forM_)
import Data.Maybe (isJust)

import Control.Lens

import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.State (MonadState)

import Reflex.Dom.Core

import GHCJS.DOM.Types (MonadJSM, JSM)

import Reflex.Test

import TodoMVC.Common
import TodoMVC.Component.ClearComplete

clearCompleteHarness ::
  MonadWidget t m =>
  m ()
clearCompleteHarness = do
  eClick <- buttonDynAttr (pure $ "id" =: "test-toggle") "Toggle"
  dToggle <- toggle False eClick
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

readTestCount ::
  ( HasTestCount s
  , MonadState s m
  , MonadJSM m
  , HasDocument m
  ) =>
  m Bool
readTestCount = do
  mi <- runMaybeT $ readOutput' "test-count"
  forM_ mi $ \i ->
    testCount . _Wrapped .= i
  pure $ isJust mi

