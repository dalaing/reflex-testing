{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Test.TextInput (
    getTextValue
  , focusText
  , blurText
  , typeText
  ) where

import Control.Monad (void, forM_)

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))

import GHCJS.DOM.Element (Element(..))
import GHCJS.DOM.HTMLElement (focus, blur)
import GHCJS.DOM.HTMLInputElement (HTMLInputElement(..), getValue, setValue)
import GHCJS.DOM.EventTarget (dispatchEvent)
import GHCJS.DOM.KeyboardEvent (newKeyboardEvent)
import GHCJS.DOM.Types (MonadJSM, liftJSM, castTo, KeyboardEventInit(..))

import GHCJS.Marshal.Pure (pFromJSVal)
import qualified Language.Javascript.JSaddle as JS

getTextValue ::
  MonadJSM m =>
  Element ->
  MaybeT m Text
getTextValue e = do
  he <- MaybeT $ castTo HTMLInputElement e
  lift $ getValue he

focusText ::
  MonadJSM m =>
  Element ->
  MaybeT m ()
focusText e = do
  he <- MaybeT $ castTo HTMLInputElement e
  lift $ focus he

blurText ::
  MonadJSM m =>
  Element ->
  MaybeT m ()
blurText e = do
  he <- MaybeT $ castTo HTMLInputElement e
  lift $ blur he

typeText ::
  MonadJSM m =>
  Text ->
  Element ->
  MaybeT m ()
typeText t e = do
  he <- MaybeT $ castTo HTMLInputElement e
  lift $ forM_ (Text.unpack t) $ \c -> do
    val <- liftJSM $ do
      obj@(JS.Object o) <- JS.create
      JS.objSetPropertyByName obj ("cancelable" :: Text) True
      JS.objSetPropertyByName obj ("bubbles" :: Text) True
      -- TODO characters to keycodes, including handling shift
      JS.objSetPropertyByName obj ("which" :: Text) (72 :: Int)
      pure $ pFromJSVal o

    let kei = Just $ KeyboardEventInit val

    keyDown <- newKeyboardEvent ("keydown" :: Text) kei
    void $ dispatchEvent he keyDown

    keyPress <- newKeyboardEvent ("keypress" :: Text) kei
    void $ dispatchEvent he keyPress

    t' <- getValue he
    setValue he $ mconcat [t', Text.pack . pure $ c]

    input <- newKeyboardEvent ("input" :: Text) kei
    void $ dispatchEvent he input

    keyUp <- newKeyboardEvent ("keyup" :: Text) kei
    void $ dispatchEvent he keyUp
