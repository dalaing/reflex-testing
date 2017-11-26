{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module List.Common (
    ModelState(..)
  , msText
  , msItems
  , fetchState
  , focusText
  , typeText
  , blurText
  , clickAdd
  , clickRemove
  ) where

import Control.Monad (void, forM_)
import Data.Maybe (isJust)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Reader (MonadReader, runReaderT)

import GHCJS.DOM.Document
import GHCJS.DOM.Node
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.EventTarget
import GHCJS.DOM.KeyboardEvent
import GHCJS.DOM.Types (MonadJSM, JSM, liftJSM, castTo, KeyboardEventInit(..))

import Reflex.Dom.Core (HasDocument)

import GHCJS.Marshal.Pure (pFromJSVal)
import qualified Language.Javascript.JSaddle as JS

import Reflex.Test
import Reflex.Test.Maybe
import Reflex.Test.Class
import Reflex.Test.Text
import Reflex.Test.Button

data ModelState (v :: * -> *) =
  ModelState {
    _msText :: Text
  , _msItems :: Seq Text
  } deriving (Eq, Ord, Show)

makeLenses ''ModelState

fetchText ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  MaybeT m Text
fetchText =
  classElementsSingle "add-input" >>= \e -> do
    he <- MaybeT $ castTo HTMLInputElement e
    lift $ getValue he

fetchItems ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  MaybeT m (Seq Text)
fetchItems = do
  xs <- classElementsMultiple "item-text" >>= traverse getText
  pure $ Seq.fromList xs

fetchState ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  MaybeT m (ModelState v)
fetchState =
  ModelState <$> fetchText <*> fetchItems

focusText ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m Bool
focusText = checkMaybe $ classElementsSingle "add-input" >>= \e -> do
  he <- MaybeT $ castTo HTMLInputElement e
  lift $ focus he

blurText ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m Bool
blurText = checkMaybe $ classElementsSingle "add-input" >>= \e -> do
  he <- MaybeT $ castTo HTMLInputElement e
  lift $ blur he

typeText ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Text ->
  m Bool
typeText t = checkMaybe $ classElementsSingle "add-input" >>= \e -> do
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

clickAdd ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m Bool
clickAdd =
  checkMaybe $ clickButton =<< classElementsSingle "add-button"

clickRemove ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Word ->
  m Bool
clickRemove i =
  checkMaybe $ clickButton =<< classElementsIx "remove-button" i
