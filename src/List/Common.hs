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
import qualified Reflex.Test.TextInput as TI

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
  classElementsSingle Nothing "add-input" >>= TI.getTextValue

fetchItems ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  MaybeT m (Seq Text)
fetchItems = do
  xs <- classElementsMultiple Nothing "item-text" >>= traverse getText
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
focusText =
  checkMaybe $ classElementsSingle Nothing "add-input" >>= TI.focusText

blurText ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m Bool
blurText =
  checkMaybe $ classElementsSingle Nothing "add-input" >>= TI.blurText

typeText ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Text ->
  m Bool
typeText t =
  checkMaybe $ classElementsSingle Nothing "add-input" >>= TI.typeText t

clickAdd ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m Bool
clickAdd =
  checkMaybe $ classElementsSingle Nothing "add-button" >>= clickButton

clickRemove ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Word ->
  m Bool
clickRemove i =
  checkMaybe $ classElementsIx Nothing "remove-button" i >>= clickButton
