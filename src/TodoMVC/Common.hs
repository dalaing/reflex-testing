{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.Common (
    todoMVCHeadSection
  , buttonDynAttr
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)

import Reflex.Dom.Core

stylesheet ::
  MonadWidget t m =>
  Text ->
  m ()
stylesheet s =
  elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", s)]) $
    pure ()

todoMVCHeadSection ::
  MonadWidget t m =>
  m ()
todoMVCHeadSection = do
  elAttr "meta" ("charset" =: "utf-8") $
    pure ()
  stylesheet "css/index.css"

buttonDynAttr ::
  MonadWidget t m =>
  Dynamic t (Map Text Text) ->
  Text ->
  m (Event t ())
buttonDynAttr dAttrs label = do
  (e, _) <- elDynAttr' "button" dAttrs $ text label
  pure $ domEvent Click e
