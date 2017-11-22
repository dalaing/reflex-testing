{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.Component.ClearComplete (
    clearComplete
  ) where

import Data.Monoid ((<>))

import Control.Lens

import Reflex.Dom.Core

import TodoMVC.Common

clearComplete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t ())
clearComplete dAny =
  let
    mkClass False = "hidden "
    mkClass True = ""
    sClass = "clear-completed "
    dClass = pure sClass <> mkClass <$> dAny
    dAttrs = ("class" =:) <$> dClass
  in
    buttonDynAttr dAttrs "Clear completed"
