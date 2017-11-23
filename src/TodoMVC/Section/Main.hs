{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module TodoMVC.Section.Main (
    mainSection
  ) where

import Data.Monoid ((<>))

import Control.Lens

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)

import Reflex.Dom.Core

import TodoMVC.Types
import TodoMVC.Component.MarkAllComplete
import TodoMVC.Component.TodoList

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
