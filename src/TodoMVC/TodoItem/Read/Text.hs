{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.TodoItem.Read.Text (
    textRead
  ) where

import Control.Monad (void)

import Data.Text (Text)

import Reflex.Dom.Core

textRead ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t ())
textRead dText = do
  (e, _) <- el' "label" . dynText $ dText
  pure . void $ domEvent Dblclick e
