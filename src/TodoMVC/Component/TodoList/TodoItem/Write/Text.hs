{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module TodoMVC.Component.TodoList.TodoItem.Write.Text (
    textWrite
  ) where

import Control.Monad (void)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex.Dom.Core

textWrite ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t Text, Event t (), Event t())
textWrite dText = mdo
  initial <- sample . current $ dText
  ti <- textInput $ def
    & textInputConfig_attributes .~ pure ("class" =: "edit")
    & textInputConfig_initialValue .~ initial
    & textInputConfig_setValue .~ leftmost [eText, updated dText]

  let
    bValue = current . fmap Text.strip $ ti ^. textInput_value
    isKey k = (== k) . keyCodeLookup . fromIntegral
    eKey = ti ^. textInput_keypress

    eEnter = void $ ffilter (isKey Enter) eKey
    eBlur = void . ffilter not . updated $ ti ^. textInput_hasFocus
    eCommit = leftmost [eEnter, eBlur]
    eAtCommit = Text.strip <$> bValue <@ eCommit
    eNewText = ffilter (not . Text.null) eAtCommit

    eRollback = void $ ffilter (isKey Escape) eKey
    eOldText = initial <$ eRollback

    eText = leftmost [eNewText, eOldText]
    eRemove = void $ ffilter Text.null eAtCommit
    eEditStop = leftmost [void eCommit, void eRollback]

  pure (eText, eRemove, eEditStop)
