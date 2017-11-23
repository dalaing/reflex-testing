{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.Section.Footer (
    footer
  ) where

import Data.Monoid ((<>))

import Data.Text (Text)

import Reflex.Dom.Core

import TodoMVC.Component.TodoCount
import TodoMVC.Component.Filter
import TodoMVC.Component.ClearComplete

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
