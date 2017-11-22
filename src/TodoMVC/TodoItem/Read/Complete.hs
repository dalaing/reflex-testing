{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.TodoItem.Read.Complete (
    complete
  ) where

import Control.Lens

import Reflex.Dom.Core

complete ::
  MonadWidget t m =>
  Event t Bool ->
  Event t () ->
  Dynamic t Bool ->
  m (Event t Bool, Event t ())
complete eMarkAllComplete eClearComplete dComplete = do
  initial <- sample . current $ dComplete
  cb <- checkbox initial $ def
    & checkboxConfig_setValue .~ eMarkAllComplete
    & checkboxConfig_attributes .~ pure ("class" =: "toggle")

  let
    eChange = leftmost [cb ^. checkbox_change, eMarkAllComplete]
    eRemove = gate (current $ cb ^. checkbox_value) eClearComplete

  pure (eChange, eRemove)
