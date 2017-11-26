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
module TodoMVC.Component.ClearComplete.Test (
    clickClearComplete
  , ClearCompleteDOMState(..)
  , HasClearCompleteDOMState(..)
  , initialClearCompleteDOMState
  , readClearCompleteDOMState
  ) where

import Control.Monad (forM)
import Data.Maybe (isJust)

import Control.Lens

import Data.Text (Text)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import qualified GHCJS.DOM.DOMTokenList as DTL
import GHCJS.DOM.Types (MonadJSM, castTo)

import Reflex.Dom.Core (HasDocument)

import Reflex.Test
import Reflex.Test.Maybe
import Reflex.Test.Class
import Reflex.Test.Button

clickClearComplete ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  m Bool
clickClearComplete =
  checkMaybe $ clickButton =<< classElementsSingle "clear-completed"

data ClearCompleteDOMState = ClearCompleteDOMState { _ccHidden :: Bool }
  deriving (Eq, Ord, Show, Read)

makeClassy ''ClearCompleteDOMState

initialClearCompleteDOMState ::
  ClearCompleteDOMState
initialClearCompleteDOMState =
  ClearCompleteDOMState False

readClearCompleteDOMState ::
  MaybeT TestJSM ClearCompleteDOMState
readClearCompleteDOMState = do
  b <- classElementsSingle "clear-completed" >>= \e -> lift $ do
    classes <- getClassList e
    DTL.contains classes ("hidden" :: Text)
  pure $ ClearCompleteDOMState b
