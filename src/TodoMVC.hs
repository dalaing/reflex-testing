{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module TodoMVC (
    todomvc
  ) where

import Control.Monad (void)
import Data.Monoid ((<>))

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core

import GHCJS.DOM.Types (JSM)

import Reflex.Dom (run)

import TodoMVC.Types
import TodoMVC.Common
import TodoMVC.Component.AddItem
import TodoMVC.Component.MarkAllComplete
import TodoMVC.Component.TodoList
import TodoMVC.Component.TodoCount
import TodoMVC.Component.Filter
import TodoMVC.Component.ClearComplete

data HeaderState (v :: * -> *) =
  HeaderState
  deriving (Eq, Ord, Show)

header ::
  MonadWidget t m =>
  m (Event t Text)
header =
  elClass "header" "header" $ do
    el "h1" $
      text "todos"
    addItem

data MainState (v :: * -> *) =
  MainState
  deriving (Eq, Ord, Show)

mainSection ::
  MonadWidget t m =>
  Map Int TodoItem ->
  Event t Text ->
  Event t () ->
  Dynamic t Filter ->
  m (Dynamic t Int, Dynamic t Bool)
mainSection initial eAdd eClearComplete dFilter = mdo
  let
    mkClass 0 = "hidden "
    mkClass _ = ""
    sClass = "main "
    dClass = pure sClass <> mkClass <$> dSize
  (dSize, dAny) <- elDynClass "section" dClass $ mdo
      let
        initialAllComplete = all (view tiComplete) initial
      eMarkAllComplete <- markAllComplete initialAllComplete dAll

      dMap <- todoList initial eAdd eMarkAllComplete eClearComplete dFilter

      let
        dSize = Map.size <$> dMap
        dAny = any (view tiComplete) <$> dMap
        dAll = all (view tiComplete) <$> dMap
      pure (dSize, dAny)

  pure (dSize, dAny)


data FooterState (v :: * -> *) =
  FooterState
  deriving (Eq, Ord, Show)

footer ::
  MonadWidget t m =>
  Dynamic t Int ->
  Dynamic t Bool ->
  m (Dynamic t Filter, Event t ())
footer dSize dAny =
  let
    mkClass 0 = "hidden "
    mkClass _ = ""
    sClass = "footer "
    dClass = pure sClass <> mkClass <$> dSize
  in
    elDynClass "footer" dClass $ do
      todoCount dSize
      dFilter <- filtersWidget
      eClearComplete <- clearComplete dAny
      pure (dFilter, eClearComplete)

todomvc ::
  Map Int TodoItem ->
  JSM ()
todomvc initial =
  mainWidgetWithHead todoMVCHeadSection $
    elClass "section" "todoapp" $ mdo
      eAdd <- header
      (dSize, dAny) <- mainSection initial eAdd eClearComplete dFilter
      (dFilter, eClearComplete) <- footer dSize dAny
      pure ()

todomvcExample ::
  JSM ()
todomvcExample =
  todomvc . Map.fromList $
    [ (1, TodoItem False "A")
    , (2, TodoItem True "B")
    , (3, TodoItem False "C")
    ]
