{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Helpers (
    buttonWithId
  , displayDivWithId
  , buttonDynAttr
  ) where

import Data.Text (Text)
import Data.Map (Map)

import Reflex.Dom.Core

buttonWithId ::
  MonadWidget t m =>
  Text ->
  Text ->
  m (Event t ())
buttonWithId eid label = do
  (e, _) <- elAttr' "button" ("id" =: eid) $ text label
  pure $ domEvent Click e

displayDivWithId ::
  ( MonadWidget t m
  , Show a
  ) =>
  Text ->
  Dynamic t a ->
  m ()
displayDivWithId eid d =
  elAttr "div" ("id" =: eid) $
    display d

buttonDynAttr ::
  MonadWidget t m =>
  Dynamic t (Map Text Text) ->
  Text ->
  m (Event t ())
buttonDynAttr dAttrs label = do
  (e, _) <- elDynAttr' "button" dAttrs $ text label
  pure $ domEvent Click e
