{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.Component.TodoCount (
    todoCount
  ) where

import Reflex.Dom.Core

todoCount ::
  MonadWidget t m =>
  Dynamic t Int ->
  m ()
todoCount dSize =
  let
    pluralize 1 = " item left"
    pluralize _ = " items left"
  in
    elClass "span" "todo-count" $ do
      el "strong" $ display dSize
      dynText $ pluralize <$> dSize
