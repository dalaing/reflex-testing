{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module List (
    listWidget
  ) where

import Control.Lens

import qualified Data.Map as Map

import Reflex.Dom.Core

import Reflex.Helpers

listWidget ::
  MonadWidget t m =>
  m ()
listWidget = mdo
  ti <- textInput $ def
          & textInputConfig_attributes .~ pure ("class" =: "add-input")
          & textInputConfig_setValue .~ ("" <$ eAdd)

  eAdd <- buttonWithClass "add-button" "Add"

  dCount <- count eAdd

  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            Map.insert <$> current dCount <*> current (ti ^. textInput_value) <@ eAdd
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmeRemoves <- el "ul" . list dMap $ \dv ->
                  elClass "li" "item" $ do
                    elClass "div" "item-text" $ dynText dv
                    buttonWithClass "remove-button" "Remove"

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemoves

  pure ()

