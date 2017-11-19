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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module List.Test (
    listStateMachine
  ) where

import Control.Monad (void, forM, forM_)
import Data.Maybe (isJust)
import Data.Typeable (Typeable)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Reader (MonadReader, ask, runReaderT)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Morph (hoist)

import Control.Monad.STM (atomically)

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element hiding (getElementsByClassName)
import GHCJS.DOM.HTMLCollection
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.EventTarget
import GHCJS.DOM.KeyboardEvent
import GHCJS.DOM.Node
import GHCJS.DOM.Types (MonadJSM, JSM, liftJSM, castTo, KeyboardEventInit(..))

import GHCJS.Marshal.Pure (pFromJSVal)
import qualified Language.Javascript.JSaddle as JS

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Internal.Property

import Reflex.Dom.Core (mainWidget, Widget)
import Reflex.Dom (run)

import Reflex.Test
import List

data ModelState (v :: * -> *) =
  ModelState {
    _msText :: Text
  , _msItems :: Seq Text
  } deriving (Eq, Ord, Show)

makeLenses ''ModelState

fetchText ::
  ( GetDocument r
  , MonadReader r m
  , MonadJSM m
  ) =>
  MaybeT m Text
fetchText =
  classElementsSingle "add-input" $ \e -> do
    he <- MaybeT $ castTo HTMLInputElement e
    lift $ getValue he

fetchItems ::
  ( GetDocument r
  , MonadReader r m
  , MonadJSM m
  ) =>
  MaybeT m (Seq Text)
fetchItems = do
  xs <- classElementsMultiple "item-text" $ \e -> do
    MaybeT . getTextContent $ e
  pure $ Seq.fromList xs

fetchState ::
  ( GetDocument r
  , MonadReader r m
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
  ( GetDocument r
  , MonadReader r m
  , MonadJSM m
  ) =>
  m Bool
focusText = do
  m <- runMaybeT $ classElementsSingle "add-input" $ \e -> do
    he <- MaybeT $ castTo HTMLInputElement e
    lift $ focus he
  pure $ isJust m

blurText ::
  ( GetDocument r
  , MonadReader r m
  , MonadJSM m
  ) =>
  m Bool
blurText = do
  m <- runMaybeT $ classElementsSingle "add-input" $ \e -> do
    he <- MaybeT $ castTo HTMLInputElement e
    lift $ blur he
  pure $ isJust m

typeText ::
  ( GetDocument r
  , MonadReader r m
  , MonadJSM m
  ) =>
  Text ->
  m Bool
typeText t = do
  m <- runMaybeT $ classElementsSingle "add-input" $ \e -> do
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

  pure $ isJust m

data Add (v :: * -> *) = Add
  deriving (Eq, Show)

instance HTraversable Add where
  htraverse _ Add = pure Add

clickAdd ::
  ( GetDocument r
  , MonadReader r m
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
  ( GetDocument r
  , MonadReader r m
  , MonadJSM m
  ) =>
  Word ->
  m Bool
clickRemove i = do
  m <- runMaybeT $ classElementsIx i "remove-button" $ \e -> do
    he <- MaybeT $ castTo HTMLElement e
    lift $ click he
  pure $ isJust m

s_type ::
  ( Monad n
  , MonadGen n
  , MonadTest m
  , MonadReader (TestingEnv (Maybe (ModelState Concrete))) m
  , MonadJSM m
  ) =>
  Command n m ModelState
s_type =
  let
    gen _ =
      Just $ Type <$> Gen.text (Range.linear 1 20) Gen.alphaNum
    execute (Type t) = do
      void $ focusText
      void $ typeText t
      void $ blurText
      waitForRender
  in
    Command gen execute [
      Update $ \s (Type t) _o ->
        s & msText %~ (`Text.append` t)
    , Ensure $ \before after (Type _) b -> do
        -- check that the state is in sync
        Just after === b
        -- check that the size of the text grows
        assert $ before ^. msText . to Text.length <= after ^. msText . to Text.length
        -- check that the list items stay the same
        assert $ before ^. msItems == after ^. msItems
    ]

s_add ::
  ( Monad n
  , MonadTest m
  , MonadReader (TestingEnv (Maybe (ModelState Concrete))) m
  , MonadJSM m
  ) =>
  Command n m ModelState
s_add =
  let
    gen st =
      if st ^. msText . to Text.length == 0
      then Nothing
      else Just $ pure Add
    execute Add = do
      void clickAdd
      waitForRender
  in
    Command gen execute [
      Require $ \s Add ->
        s ^. msText . to Text.length /= 0
    , Update $ \s Add _o ->
          s & msItems %~ (\i -> i Seq.|> (s ^. msText))
            & msText .~ ""
    , Ensure $ \before after Add b -> do
        -- check that the state is in sync
        Just after === b

        -- check that the text becomes empty
        assert . Text.null $ after ^. msText
        -- check that the size of the list grows
        assert $ before ^. msItems . to Seq.length <= after ^. msItems . to Seq.length

        -- check that the value in the before text box is added to the items
        let
          text = before ^. msText
          count = Seq.length . Seq.filter (== text)
        assert $ before ^. msItems . to count + 1 == after ^. msItems . to count
    ]

s_remove ::
  ( Monad n
  , MonadGen n
  , MonadTest m
  , MonadReader (TestingEnv (Maybe (ModelState Concrete))) m
  , MonadJSM m
  ) =>
  Command n m ModelState
s_remove =
  let
    gen st =
      let
        l = st ^. msItems . to Seq.length
      in
        if l == 0
        then Nothing
        else Just $ Remove <$> Gen.int (Range.linear 0 (l - 1))
    execute (Remove i) = do
      void $ clickRemove (fromIntegral i)
      waitForRender
  in
    Command gen execute [
      Require $ \s (Remove i) ->
        0 <= i && i < s ^. msItems . to Seq.length
    , Update $ \s (Remove i) _o ->
        s & msItems %~ \s -> (Seq.take i s) Seq.>< (Seq.drop (i + 1) s)
    , Ensure $ \before after (Remove i) b -> do
        -- check that the state is in sync
        Just after === b

        -- check that the size of the text stays the same
        assert $ before ^. msText . to Text.length == after ^. msText . to Text.length
        -- check that the size of the list shrinks
        assert $ before ^. msItems . to Seq.length >= after ^. msItems . to Seq.length

        -- check that the item is removed
        let
          text = before ^. msItems . to (`Seq.index` i)
          count = Seq.length . Seq.filter (== text)
        assert $ before ^. msItems . to count == after ^. msItems . to count + 1
    ]

initialState ::
  ModelState v
initialState =
  ModelState "" Seq.empty

listStateMachine ::
  PropertyT JSM ()
listStateMachine = do
  env <- liftIO . atomically $ mkTestingEnv
  _ <- lift $ do
    mainWidget $ testingWidget fetchState' env $ listWidget
    runReaderT resetTest env

  let
    hoistEnv = hoistCommand (hoist $ hoist $ unTestJSM . flip runReaderT env)

  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialResettableState [
      hoistEnv $ s_reset initialState
    , hoistEnv $ prismCommand _Running s_type
    , hoistEnv $ prismCommand _Running s_add
    , hoistEnv $ prismCommand _Running s_remove
    ]
  PropertyT $ executeSequential initialResettableState actions
