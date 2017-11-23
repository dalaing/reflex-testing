{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.Section.Header (
    header
  ) where

import Data.Text (Text)

import Reflex.Dom.Core

import TodoMVC.Component.AddItem

header ::
  MonadWidget t m =>
  m (Event t Text)
header =
  elClass "header" "header" $ do
    el "h1" $
      text "todos"
    addItem
