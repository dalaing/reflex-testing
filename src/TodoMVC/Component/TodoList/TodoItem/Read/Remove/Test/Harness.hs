{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.Component.TodoList.TodoItem.Read.Remove.Test.Harness (
  ) where

import Reflex.Dom.Core

import TodoMVC.Component.TodoList.TodoItem.Read.Remove
import TodoMVC.Component.TodoList.TodoItem.Read.Remove.Test

removeHarness ::
  MonadWidget t m =>
  m ()
removeHarness = do
  eRemove <- remove
  dCount <- count eRemove
  elAttr "div" ("id" =: "remove-count") $
    display dCount
