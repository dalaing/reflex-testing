{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.Component.TodoList.TodoItem.Read.Text.Test (
    dblclick
  ) where

import Control.Monad (void)

import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Text (Text)

import Reflex.Dom.Core (HasDocument)

import GHCJS.DOM.Element (Element(..))
import GHCJS.DOM.HTMLElement (HTMLElement(..))
import GHCJS.DOM.EventTarget (dispatchEvent)
import GHCJS.DOM.MouseEvent (newMouseEvent)
import GHCJS.DOM.Types (MonadJSM, liftJSM, castTo, MouseEventInit(..))

import GHCJS.Marshal.Pure (pFromJSVal)
import qualified Language.Javascript.JSaddle as JS

dblclick ::
  MonadJSM m =>
  Element ->
  MaybeT m ()
dblclick e = do
  he <- MaybeT $ castTo HTMLElement e
  val <- liftJSM $ do
    obj@(JS.Object o) <- JS.create
    JS.objSetPropertyByName obj ("cancelable" :: Text) True
    JS.objSetPropertyByName obj ("bubbles" :: Text) True
    pure $ pFromJSVal o

  let mei = Just $ MouseEventInit val
  dblclick <- newMouseEvent ("dblclick" :: Text) mei

  void $ dispatchEvent he dblclick
