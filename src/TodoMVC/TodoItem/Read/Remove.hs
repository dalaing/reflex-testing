{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.TodoItem.Read.Remove (
    remove
  ) where

import Reflex.Dom.Core

import TodoMVC.Common

remove ::
  MonadWidget t m =>
  m (Event t ())
remove =
  buttonDynAttr (pure $ "class" =: "destroy") ""
