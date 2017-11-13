{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Todo.Item.Remove (
    remove
  ) where

import Reflex.Dom.Core

import Reflex.Helpers

remove ::
  MonadWidget t m =>
  m (Event t ())
remove =
  buttonDynAttr (pure $ "class" =: "remove") ""

-- helper function to toggle the button

-- clicking the button causes the event to fire
