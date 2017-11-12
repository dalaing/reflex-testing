{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings#-}
module Todo (
  ) where

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core

remove ::
  MonadWidget t m =>
  m (Event t ())
remove = do
  (e, _) <- elClass "button" "remove-btn" $ text "Remove"
  pure $ domEvent Click e

complete
  MonadWidget t m =>
  Bool ->
  Event t Bool ->
  Event t () ->
  m (Dynamic t Bool, Event t ())
complete initial eMarkAllComplete eClearComplete = do
  cb <- checkbox initial $ def
    & checkboxConfig_setValue .~ eMarkAllComplete
    & checkboxConfig_attributes .~ pure ("class" =: "complete-cb")

  let
    dComplete = cb ^. checkbox_value
    eRemove = gate (current dComplete) eClearComplete

  pure (dComplete, eRemove)

textRead ::
  MonadWidget t m =>
  Text ->
  m (Event t ())
textRead initial = do
  (e, _) <- elClass "div" "text-read" $ text initial
  pure $ domEvent Dblclick e

textWrite ::
  MonadWidget t m =>
  Text ->
  m (Event t (), Event Text)
textWrite initial = do
  ti <- textInput $ def
    & textInputConfig_attributes .~ pure ("class" =: "text-write")
    & textInputConfig_initialValue .~ initial
    & textInputConfig_setValue .~ initial <$ eRollback

  let
    bValue = current . fmap Text.strip $ ti ^. textInput_value

    isKey k = (== k) . keyCodeLookup . fromIntegral
    eKey = ti ^. textInput_keypress
    eEnter = void . ffilter (isKey Enter) $ eKey
    eEscape = void . ffilter (isKey Escape) $ eKey

    -- - want to use a blur event here, either to
    --   combine with eEnter or eRollback
    -- - will need to switch it out or it'll have some funky
    --   effects in a workflow situation
    -- - possibly a good demo for the testing

    eAtEnter = bValue <@ eEnter

    eCommit = ffilter (not . Text.null) eAtEnter
    eRemove = ffilter Text.null eConfirmValue
    eRollback = eEscape

  pure (eRemove, eCommit)
