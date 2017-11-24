{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module TodoMVC.Component.TodoCount.Test (
    TodoCountDOMState(..)
  , HasTodoCountDOMState(..)
  , initialTodoCountDOMState
  , readTodoCountDOMState
  ) where

import Text.Read

import Control.Lens

import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Text (Text)
import qualified Data.Text as Text

import GHCJS.DOM.Element
import GHCJS.DOM.Node

import Reflex.Test

data TodoCountDOMState =
  TodoCountDOMState {
    _tcCount :: Int
  , _tcText :: Text
  } deriving (Eq, Show)

makeClassy ''TodoCountDOMState

initialTodoCountDOMState ::
  TodoCountDOMState
initialTodoCountDOMState =
  TodoCountDOMState 0 " items left"

readTodoCountDOMState ::
  MaybeT TestJSM TodoCountDOMState
readTodoCountDOMState =
  classElementsSingle "todo-count" $ \e -> do
    i1 <- MaybeT . getFirstChild $ e
    tc <- MaybeT . getTextContent $ i1
    c <- MaybeT . pure . readMaybe . Text.unpack $ tc
    i2 <- MaybeT . getNextSibling $ i1
    t <- MaybeT . getTextContent $ i2
    pure $ TodoCountDOMState c t
