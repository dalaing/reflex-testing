{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module TodoMVC.TodoItem.Write (
    todoItemWrite
  ) where

import Control.Lens

import Reflex.Dom.Core

import TodoMVC.Types
import TodoMVC.TodoItem.Write.Text

todoItemWrite ::
  MonadWidget t m =>
  Dynamic t TodoItem ->
  m (Event t (TodoItem -> TodoItem), Event t (), Event t())
todoItemWrite dItem = do
  (eText, eRemove, eEditStop) <- textWrite $ view tiText <$> dItem
  pure (set tiText <$> eText, eRemove, eEditStop)
