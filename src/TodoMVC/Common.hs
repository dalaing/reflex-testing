{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.Common (
    buttonDynAttr
  ) where

import Data.Map (Map)
import Data.Text (Text)

import Reflex.Dom.Core

buttonDynAttr ::
  MonadWidget t m =>
  Dynamic t (Map Text Text) ->
  Text ->
  m (Event t ())
buttonDynAttr dAttrs label = do
  (e, _) <- elDynAttr' "button" dAttrs $ text label
  pure $ domEvent Click e
