{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
module TodoMVC.Types.TodoItem (
    TodoItem(..)
  , tiComplete
  , tiText
  ) where

import Control.Lens

import Data.Text (Text)

data TodoItem =
  TodoItem {
    _tiComplete :: Bool
  , _tiText :: Text
  } deriving (Eq, Ord, Show)

makeLenses ''TodoItem
