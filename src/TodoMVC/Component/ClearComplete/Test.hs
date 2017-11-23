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

import Control.Monad (forM)
import Data.Maybe (isJust)

import Control.Lens

import Data.Text (Text)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import qualified GHCJS.DOM.DOMTokenList as DTL
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

readClearCompleteDOMState :: MaybeT TestJSM ClearCompleteDOMState
readClearCompleteDOMState = do
  b <- classElementsSingle "clear-completed" $ \e -> lift $ do
    classes <- getClassList e
    DTL.contains classes ("hidden" :: Text)
  pure $ ClearCompleteDOMState b
