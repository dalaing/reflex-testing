{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.Component.MarkAllComplete.Test.Harness (
  ) where

import Reflex.Dom.Core

import Reflex.Test

import TodoMVC.Common
import TodoMVC.Component.MarkAllComplete
import TodoMVC.Component.MarkAllComplete.Test

markAllCompleteHarness ::
  MonadWidget t m =>
  m ()
markAllCompleteHarness = do
  eClick <- buttonDynAttr (pure $ "id" =: "test-toggle") "Toggle"
  dToggle <- toggle True eClick
  eComplete <- markAllComplete False dToggle
  dComplete <- holdDyn False eComplete
  elAttr "div" ("id" =: "test-complete") $
    display dComplete

  pure ()
