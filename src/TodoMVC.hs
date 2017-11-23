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

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core

import GHCJS.DOM.Types (JSM)

import Reflex.Dom (run)

import TodoMVC.Types
import TodoMVC.Common
import TodoMVC.Section.Header
import TodoMVC.Section.Main
import TodoMVC.Section.Footer

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
