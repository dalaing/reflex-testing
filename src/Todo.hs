{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE RecursiveDo #-}
module Todo (
  ) where

import Control.Monad (void)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core

textRead ::
  MonadWidget t m =>
  Text ->
  m (Event t ())
textRead initial = do
  (e, _) <- elClass' "div" "text-read" $ text initial
  pure . void $ domEvent Dblclick e

textWrite ::
  MonadWidget t m =>
  Text ->
  m (Event t (), Event t Text)
textWrite initial = mdo
  ti <- textInput $ def
    & textInputConfig_attributes .~ pure ("class" =: "text-write")
    & textInputConfig_initialValue .~ initial
    & textInputConfig_setValue .~ (initial <$ eRollback)

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
    eRemove = void $ ffilter Text.null eAtEnter
    eRollback = void eEscape

  pure (eRemove, eCommit)

data TodoItem =
  TodoItem {
    tiComplete :: Bool
  , tiText :: Text
  }

-- workflow version and hide / show version


