{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module TodoMVC.TodoItem (
    todoItem
  ) where

import Data.Monoid ((<>))

import Control.Lens

import Reflex.Dom.Core

import TodoMVC.Types
import TodoMVC.Filter
import TodoMVC.TodoItem.Read
import TodoMVC.TodoItem.Write

todoItem ::
  MonadWidget t m =>
  Event t Bool ->
  Event t () ->
  Dynamic t Filter ->
  Dynamic t TodoItem ->
  m (Event t (TodoItem -> TodoItem), Event t ())
todoItem eMarkAllComplete eClearComplete dFilter dItem = mdo
  let
    dComplete = view tiComplete <$> dItem
    mkCompleted False = ""
    mkCompleted True = "completed "
    dCompleted = mkCompleted <$> dComplete
    mkEditing False = ""
    mkEditing True = "editing "
    dEditing = mkEditing <$> dEdit
    mkHidden False = "hidden "
    mkHidden True = ""
    dMatches = filterShowComplete <$> dFilter <*> dComplete
    dHidden = mkHidden <$> dMatches

  dEdit <- holdDyn False eEdit
  (eChange, eRemove, eEdit) <- elDynClass "li" (dCompleted <> dEditing <> dHidden) $ mdo

    (eChangeRead, eRemoveRead, eEditStart) <- todoItemRead eMarkAllComplete eClearComplete dItem
    (eChangeWrite, eRemoveWrite, eEditStop) <- todoItemWrite dItem

    let
      eChange = mergeWith (.) [eChangeRead, eChangeWrite]
      eRemove = leftmost [eRemoveRead, eRemoveWrite]
      eEdit = leftmost [True <$ eEditStart, False <$ eEditStop]

    pure (eChange, eRemove, eEdit)

  pure (eChange, eRemove)
