{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module List.Test (
  ) where

import Control.Monad (void, forM, forM_)
import Data.Maybe (isJust)

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Reader (MonadReader, ask, runReaderT)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Data.Sequence (Seq)
import Data.Sequence as Seq

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element hiding (getElementsByClassName)
import GHCJS.DOM.HTMLCollection
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.EventTarget
import GHCJS.DOM.KeyboardEvent
import GHCJS.DOM.Node
import GHCJS.DOM.Types (MonadJSM, JSM, castTo)

import Reflex.Dom (run)

import Hedgehog

import Reflex.Test
import List

data ModelState (v :: * -> *) =
  ModelState {
    msText :: Text
  , msItems :: Seq Text
  } deriving (Eq, Ord, Show)

fetchText ::
  ( MonadReader Document m
  , MonadJSM m
  ) =>
  MaybeT m Text
fetchText = classElementsSingle "add-input" $ \e -> do
  he <- MaybeT $ castTo HTMLInputElement e
  lift $ getValue he

fetchItems ::
  ( MonadReader Document m
  , MonadJSM m
  ) =>
  MaybeT m (Seq Text)
fetchItems = do
  xs <- classElementsMultiple "item-text" $ \e -> do
    MaybeT . getTextContent $ e
  pure $ Seq.fromList xs

fetchState ::
  ( MonadReader Document m
  , MonadJSM m
  ) =>
  MaybeT m (ModelState v)
fetchState =
  ModelState <$> fetchText <*> fetchItems

fetchState' ::
  Document ->
  JSM (Maybe (ModelState v))
fetchState' =
  runReaderT (runMaybeT fetchState)

data Type (v :: * -> *) = Type Text
  deriving (Eq, Show)

instance HTraversable Type where
  htraverse _ (Type t) = pure (Type t)

focusText ::
  ( MonadReader Document m
  , MonadJSM m
  ) =>
  m Bool
focusText = do
  m <- runMaybeT $ classElementsSingle "add-input" $ \e -> do
    he <- MaybeT $ castTo HTMLInputElement e
    lift $ focus he
  pure $ isJust m

blurText ::
  ( MonadReader Document m
  , MonadJSM m
  ) =>
  m Bool
blurText = do
  m <- runMaybeT $ classElementsSingle "add-input" $ \e -> do
    he <- MaybeT $ castTo HTMLInputElement e
    lift $ blur he
  pure $ isJust m

typeText ::
  ( MonadReader Document m
  , MonadJSM m
  ) =>
  Text ->
  m Bool
typeText t = do
  m <- runMaybeT $ classElementsSingle "add-input" $ \e -> do
    he <- MaybeT $ castTo HTMLInputElement e
    -- need to dispatch the key up and key down events to this
    -- lift $ setValue he t
    lift $ forM_ (Text.unpack t) $ \c -> do

      keyDown <- newKeyboardEvent ("keydown" :: Text) Nothing
      dispatchEvent he keyDown

      keyPress <- newKeyboardEvent ("keypress" :: Text) Nothing
      dispatchEvent he keyPress

      keyUp <- newKeyboardEvent ("keyup" :: Text) Nothing
      dispatchEvent he keyUp

  pure $ isJust m

data Add (v :: * -> *) = Add
  deriving (Eq, Show)

instance HTraversable Add where
  htraverse _ Add = pure Add

clickAdd ::
  ( MonadReader Document m
  , MonadJSM m
  ) =>
  m Bool
clickAdd = do
  m <- runMaybeT $ classElementsSingle "add-button" $ \e -> do
    he <- MaybeT $ castTo HTMLElement e
    lift $ click he
  pure $ isJust m

data Remove (v :: * -> *) = Remove Int
  deriving (Eq, Show)

instance HTraversable Remove where
  htraverse _ (Remove i) = pure (Remove i)

clickRemove ::
  ( MonadReader Document m
  , MonadJSM m
  ) =>
  Word ->
  m Bool
clickRemove i = do
  m <- runMaybeT $ classElementsIx i "remove-button" $ \e -> do
    he <- MaybeT $ castTo HTMLElement e
    lift $ click he
  pure $ isJust m

boo :: IO ()
boo = run $ do
  env <- mkTestingEnv fetchState' listWidget

  flip runReaderT (teDocument env) $ do
    void $ focusText
    void $ typeText "a"
    void $ blurText

    void clickAdd

    void $ focusText
    void $ typeText "b"
    void $ blurText

    void clickAdd

    void $ focusText
    void $ typeText "c"
    void $ blurText

    void clickAdd

    void $ focusText
    void $ typeText "d"
    void $ blurText

    void $ clickRemove 1
