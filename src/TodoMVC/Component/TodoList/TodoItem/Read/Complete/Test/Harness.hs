{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.Component.TodoList.TodoItem.Read.Complete.Test.Harness (
  ) where

import Reflex.Dom.Core hiding (Command)

import TodoMVC.Common
import TodoMVC.Component.TodoList.TodoItem.Read.Complete
import TodoMVC.Component.TodoList.TodoItem.Read.Complete.Test

completeTestHarness ::
  MonadWidget t m =>
  m ()
completeTestHarness = do
  eToggleMarkAllComplete <- buttonDynAttr (pure $ "id" =: "mark-all-complete-btn") "Clear complete"
  dToggleMarkAllComplete <- toggle False eToggleMarkAllComplete
  elAttr "div" ("id" =: "test-mark-all-complete") $
    display dToggleMarkAllComplete
  let
    eMarkAllComplete = updated dToggleMarkAllComplete

  eClearComplete <- buttonDynAttr (pure $ "id" =: "clear-complete-btn") "Clear complete"

  (eChange, eRemove) <- complete eMarkAllComplete eClearComplete (pure False)

  dChange <- holdDyn False eChange
  elAttr "div" ("id" =: "test-change") $
    display dToggleMarkAllComplete

  dRemove <- count eRemove
  elAttr "div" ("id" =: "test-remove") $
    display dToggleMarkAllComplete

  pure ()
