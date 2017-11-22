{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.Component.TodoList.TodoItem.Read (
    todoItemRead
  ) where

import Control.Lens

import Reflex.Dom.Core

import TodoMVC.Types
import TodoMVC.Component.TodoList.TodoItem.Read.Complete
import TodoMVC.Component.TodoList.TodoItem.Read.Text
import TodoMVC.Component.TodoList.TodoItem.Read.Remove

todoItemRead ::
  MonadWidget t m =>
  Event t Bool ->
  Event t () ->
  Dynamic t TodoItem ->
  m (Event t (TodoItem -> TodoItem), Event t (), Event t())
todoItemRead eMarkAllComplete eClearComplete dItem = elClass "div" "view" $ do
  (eComplete, eRemoveComplete) <- complete eMarkAllComplete eClearComplete $ view tiComplete <$> dItem

  eEditStart <- textRead $ view tiText <$> dItem
  eRemoveClick <- remove

  let
    eChange = set tiComplete <$> eComplete
    eRemove = leftmost [eRemoveClick, eRemoveComplete]

  pure (eChange, eRemove, eEditStart)
