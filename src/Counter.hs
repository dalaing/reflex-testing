{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Counter (
    counter
  ) where

import Reflex.Dom.Core

import Reflex.Helpers

counter ::
  MonadWidget t m =>
  m ()
counter = do
  eAdd   <- buttonWithId "add-btn" "Add"
  eClear <- buttonWithId "clear-btn" "Clear"

  dCount <- foldDyn ($) 0 . mergeWith (.) $ [
              succ    <$ eAdd
            , const 0 <$ eClear
            ]

  displayDivWithId "count-output" dCount

  pure ()
