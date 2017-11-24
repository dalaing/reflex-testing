{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module TodoMVC.Component.Filter.Test (
    clickFilter
  , FiltersDOMState(..)
  , HasFiltersDOMState(..)
  , initialFiltersDOMState
  , readFiltersDOMState
  ) where

import Control.Monad (forM_, forM)
import Data.Maybe (isJust)

import Control.Lens

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)

import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.HTMLCollection
import GHCJS.DOM.Node
import qualified GHCJS.DOM.DOMTokenList as DTL
import GHCJS.DOM.Types (MonadJSM, castTo)

import Reflex.Dom.Core (HasDocument)

import Reflex.Test

import TodoMVC.Types.Filter

clickFilter ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Filter ->
  m Bool
clickFilter f = do
  m <- runMaybeT $ classElementsSingle "filters" $ \e -> do
    links <- lift $ getElementsByTagName e ("a" :: Text)
    l <- getLength links
    forM_ [0..l-1] $ \i -> do
      e' <- MaybeT . item links $ i
      t <- MaybeT . getTextContent $ e'
      if (t == filterLabel f)
      then do
        he <- MaybeT $ castTo HTMLElement e'
        lift $ click he
      else pure ()
    pure ()
  pure $ isJust m

data FiltersDOMState =
  FiltersDOMState {
    _fSelected :: Set Filter
  } deriving (Eq, Show)

makeClassy ''FiltersDOMState

initialFiltersDOMState ::
  FiltersDOMState
initialFiltersDOMState =
  FiltersDOMState Set.empty

readFiltersDOMState ::
  MaybeT TestJSM FiltersDOMState
readFiltersDOMState =
  classElementsSingle "filters" $ \e -> do
    links <- lift $ getElementsByTagName e ("a" :: Text)
    l <- getLength links
    s <- forM [0..l-1] $ \i -> do
      e' <- MaybeT . item links $ i
      classes <- getClassList e'
      t <- MaybeT . getTextContent $ e'
      f <- MaybeT . pure . parseFilterLabel $ t
      b <- DTL.contains classes ("selected" :: Text)
      pure $ if b
             then Set.singleton f
             else Set.empty
    pure . FiltersDOMState . mconcat $ s


