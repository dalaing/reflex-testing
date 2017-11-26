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

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Internal.Property

import Reflex.Dom.Core (mainWidget, Widget, HasDocument)
import Reflex.Dom (run)

import Reflex.Test
import Reflex.Test.Hedgehog

import List
import List.Common

data Type (v :: * -> *) = Type Text
  deriving (Eq, Show)

instance HTraversable Type where
  htraverse _ (Type t) = pure (Type t)

data Add (v :: * -> *) = Add
  deriving (Eq, Show)

instance HTraversable Add where
  htraverse _ Add = pure Add

data Remove (v :: * -> *) = Remove Int
  deriving (Eq, Show)

instance HTraversable Remove where
  htraverse _ (Remove i) = pure (Remove i)

s_type ::
  ( Monad n
  , MonadGen n
  , MonadTest m
  , MonadReader (TestingEnv (ModelState Concrete)) m
  , MonadJSM m
  , HasDocument m
  ) =>
  Command n m ModelState
s_type =
  let
    gen _ =
      Just $ Type <$> Gen.text (Range.linear 1 20) Gen.alphaNum
    execute (Type t) = do
      void focusText
      void $ typeText t
      void blurText
      waitForRender
  in
    Command gen execute [
      Update $ \s (Type t) _o ->
        s & msText %~ (`Text.append` t)
    , Ensure $ \before after (Type _) b -> do
        -- check that the state is in sync
        after === b
        -- check that the size of the text grows
        assert $ before ^. msText . to Text.length <= after ^. msText . to Text.length
        -- check that the list items stay the same
        assert $ before ^. msItems == after ^. msItems
    ]

s_add ::
  ( Monad n
  , MonadTest m
  , MonadReader (TestingEnv (ModelState Concrete)) m
  , MonadJSM m
  , HasDocument m
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
        after === b

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
  , MonadReader (TestingEnv (ModelState Concrete)) m
  , MonadJSM m
  , HasDocument m
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
        -- broken version to demonstrate
        -- if i == 0 then s else s & msItems %~ \s -> (Seq.take (i - 1) s) Seq.>< (Seq.drop i s)
    , Ensure $ \before after (Remove i) b -> do
        -- check that the state is in sync
        after === b

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
  env <- lift $ setupResettableWidget fetchState listWidget
  runSequentialResettable env (Range.linear 1 100) initialState [
      s_type
    , s_add
    , s_remove
    ]
