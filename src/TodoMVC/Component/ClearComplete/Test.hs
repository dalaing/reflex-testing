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
module TodoMVC.Component.ClearComplete.Test (
    clickClearComplete
  , ClearCompleteDOMState(..)
  , HasClearCompleteDOMState(..)
  , readClearCompleteDOMState
  ) where

import Control.Monad (forM_)
import Data.Maybe (isJust)

import Control.Lens

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.State (MonadState)

import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Types (MonadJSM, castTo)

import Reflex.Dom.Core (HasDocument)

import Reflex.Test

clickClearComplete ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m Bool
clickClearComplete = do
  m <- runMaybeT $ classElementsSingle "clear-completed" $ \e -> do
    he <- MaybeT $ castTo HTMLElement e
    lift $ click he
  pure $ isJust m

data ClearCompleteDOMState = ClearCompleteDOMState { _ccHidden :: Bool }
  deriving (Eq, Ord, Show, Read)

makeClassy ''ClearCompleteDOMState

readClearCompleteDOMState ::
  ( HasClearCompleteDOMState s
  , MonadState s m
  , MonadJSM m
  , HasDocument m
  ) =>
  m Bool
readClearCompleteDOMState = do
  mb <- runMaybeT $ classElementsSingle "clear-completed" $ \e -> do
    -- TODO check e for the hidden class
    pure False
  forM_ mb $ \b ->
    ccHidden .= b
  pure $ isJust mb

-- TODO add action here
