{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module TodoMVC.Component.TodoList (
    todoList
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)

import Reflex.Dom.Core

import TodoMVC.Types
import TodoMVC.Component.TodoList.TodoItem

todoList ::
  MonadWidget t m =>
  Map Int TodoItem ->
  Event t Text ->
  Event t Bool ->
  Event t () ->
  Dynamic t Filter ->
  m (Dynamic t (Map Int TodoItem))
todoList initial eAdd eMarkAllComplete eClearComplete dFilter = mdo
  dCount <- count eAdd
  let
    initialCount = (+ 1) . maximum . (0 :) . Map.keys $ initial
    dKey = (+ initialCount) <$> dCount
    eItem = TodoItem False <$> eAdd

  dMap <- foldDyn ($) initial . mergeWith (.) $ [
            Map.insert <$> current dKey <@> eItem
          , Map.mergeWithKey (\_ f x -> Just (f x)) (const mempty) id <$> eChanges
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dOut <- elClass "ul" "todo-list" . list dMap $
    todoItem eMarkAllComplete eClearComplete dFilter

  let
    eChanges = switch . current . fmap (mergeMap . fmap fst) $ dOut
    eRemoves = fmap Map.keys . switch . current . fmap (mergeMap . fmap snd) $ dOut

  pure dMap
