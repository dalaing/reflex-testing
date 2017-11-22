{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.Component.MarkAllComplete (
    markAllComplete
  ) where

import Data.Monoid ((<>))

import Control.Lens

import Reflex.Dom.Core

markAllComplete ::
  MonadWidget t m =>
  Bool ->
  Dynamic t Bool ->
  m (Event t Bool)
markAllComplete initialAllComplete dAll = do
  cb <- checkbox initialAllComplete $ def
    & checkboxConfig_setValue .~ updated dAll
    & checkboxConfig_attributes .~ pure ("id" =: "toggle-all" <> "class" =: "toggle-all")
  elAttr "label" ("for" =: "toggle-all") $ text "Mark all as complete"
  pure $ cb ^. checkbox_change
