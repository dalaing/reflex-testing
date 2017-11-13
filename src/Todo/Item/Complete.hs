{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Todo.Item.Complete (
    complete
  ) where

import Control.Lens

import Reflex.Dom.Core

complete ::
  MonadWidget t m =>
  Bool ->
  Event t Bool ->
  Event t () ->
  m (Dynamic t Bool, Event t ())
complete initial eMarkAllComplete eClearComplete = do
  cb <- checkbox initial $ def
    & checkboxConfig_setValue .~ eMarkAllComplete
    & checkboxConfig_attributes .~ pure ("class" =: "toggle")

  let
    dComplete = cb ^. checkbox_value
    eRemove = gate (current dComplete) eClearComplete

  pure (dComplete, eRemove)

-- helper function to grab the state
-- helper function to toggle the button

-- clicking the checkbox changes the completion status
-- mark all complete sets the completion status
-- clear complete triggers remove if the completion status is set
-- clear complete does not trigger remove if the completion status is not set
