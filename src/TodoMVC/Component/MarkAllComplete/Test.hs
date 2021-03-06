{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TodoMVC.Component.MarkAllComplete.Test (
    toggleMarkAllComplete
  , MarkAllCompleteDOMState(..)
  , HasMarkAllCompleteDOMState(..)
  , initialMarkAllCompleteDOMState
  , readMarkAllCompleteDOMState
  ) where

import Control.Monad (forM)
import Data.Maybe (isJust)

import Control.Lens

import Data.Text (Text)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.Types (MonadJSM, castTo)

import Reflex.Dom.Core (HasDocument, askDocument)

import Reflex.Test
import Reflex.Test.Maybe
import Reflex.Test.Id
import Reflex.Test.Checkbox

toggleMarkAllComplete ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m Bool
toggleMarkAllComplete =
  checkMaybe $ idElement "toggle-all" >>= \e ->
    toggleChecked e

data MarkAllCompleteDOMState = MarkAllCompleteDOMState { _macChecked :: Bool }
  deriving (Eq, Ord, Show, Read)

makeClassy ''MarkAllCompleteDOMState

initialMarkAllCompleteDOMState ::
  MarkAllCompleteDOMState
initialMarkAllCompleteDOMState =
  MarkAllCompleteDOMState False

readMarkAllCompleteDOMState ::
  MaybeT TestJSM MarkAllCompleteDOMState
readMarkAllCompleteDOMState =
  idElement "toggle-all" >>= \e -> do
    b <- getChecked e
    pure $ MarkAllCompleteDOMState b
