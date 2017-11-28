{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.Component.TodoList.TodoItem.Read.Remove.Test (
    clickRemove
  ) where

import Reflex.Dom.Core (HasDocument)
import GHCJS.DOM.Types (MonadJSM)

import Reflex.Test.Maybe
import Reflex.Test.Class
import Reflex.Test.Button

clickRemove ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m Bool
clickRemove =
  checkMaybe $ classElementsSingle Nothing "destroy" >>= clickButton
