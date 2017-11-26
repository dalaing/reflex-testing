{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Reflex.Test.Class (
    classElementsSingle
  , classElementsMultiple
  , classElementsIx
  ) where

import Control.Monad (forM)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Text (Text)

import Reflex.Dom.Core (HasDocument(..))

import GHCJS.DOM.Document (getElementsByClassName)
import GHCJS.DOM.Element (Element)
import GHCJS.DOM.HTMLCollection (HTMLCollection(..), getLength, item)
import GHCJS.DOM.Types (MonadJSM)

classElements ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Text ->
  m (Word, HTMLCollection)
classElements eclass = do
  doc <- askDocument
  c <- getElementsByClassName doc eclass
  l <- getLength c
  pure (l, c)

classElementsSingle ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Text ->
  MaybeT m Element
classElementsSingle eclass = do
  doc <- lift askDocument
  (l, c) <- lift $ classElements eclass
  MaybeT $
    if (l /= 1)
    then pure Nothing
    else item c 0

classElementsMultiple ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Text ->
  MaybeT m [Element]
classElementsMultiple eclass = do
  (l, c) <- lift $ classElements eclass
  if (l == 0)
  then pure []
  else forM [0..l-1] $ MaybeT . item c

classElementsIx ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Text ->
  Word ->
  MaybeT m Element
classElementsIx eclass i = do
  (l, c) <- lift $ classElements eclass
  MaybeT $
    if (i < 0 || l <= i)
    then pure Nothing
    else item c i
