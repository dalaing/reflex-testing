{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Todo.Test (
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Todo

data ItemState (v :: * -> *) =
  ItemState {
    isComplete :: Bool
  , isText :: Text
  , isEdit :: Bool
  } deriving (Eq, Ord, Show)

data ListState (v :: * -> *) =
  ListState {
    lsItems :: Map Int (ItemState v)
  , lsAllComplete :: Bool
  , lsAddFocus :: Bool
  } deriving (Eq, Ord, Show)

-- s_add_type
-- s_add_clear
-- s_add_enter
-- s_add_blur
-- s_add_focus
-- s_markAllComplete
-- s_clearComplete
-- si_remove
-- si_complete
-- si_start_edit
-- si_edit_type
-- si_edit_clear
-- si_edit_enter
-- si_edit_escape
-- si_edit_blur
