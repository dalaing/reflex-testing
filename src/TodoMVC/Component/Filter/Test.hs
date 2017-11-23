{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.Component.Filter.Test (
    clickFilter
  ) where

import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import qualified GHCJS.DOM.DOMTokenList as DTL
import GHCJS.DOM.Types (MonadJSM, castTo)

import Reflex.Dom.Core (HasDocument)

import TodoMVC.Types.Filter

clickFilter ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Filter ->
  m Bool
clickFilter f =
  {-
  m <- runMaybeT $ classElementsSingle "clear-completed" $ \e -> do
    he <- MaybeT $ castTo HTMLElement e
    lift $ click he
  pure $ isJust m
  -}
  pure False
