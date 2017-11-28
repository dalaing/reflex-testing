{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Reflex.Test.Tag (
    tagElementsSingle
  , tagElementsMultiple
  , tagElementsIx
  ) where

import Control.Monad (forM)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Text (Text)

import Reflex.Dom.Core (HasDocument(..))

import qualified GHCJS.DOM.Document as Document (getElementsByTagName)
import GHCJS.DOM.Element (Element)
import qualified GHCJS.DOM.Element as Element (getElementsByTagName)
import GHCJS.DOM.HTMLCollection (HTMLCollection(..), getLength, item)
import GHCJS.DOM.Types (MonadJSM)

tagElements ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Maybe Element ->
  Text ->
  m (Word, HTMLCollection)
tagElements Nothing etag = do
  doc <- askDocument
  c <- Document.getElementsByTagName doc etag
  l <- getLength c
  pure (l, c)
tagElements (Just e) etag = do
  c <- Element.getElementsByTagName e etag
  l <- getLength c
  pure (l, c)

tagElementsSingle ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Maybe Element ->
  Text ->
  MaybeT m Element
tagElementsSingle mParent etag = do
  (l, c) <- lift $ tagElements mParent etag
  MaybeT $
    if (l /= 1)
    then pure Nothing
    else item c 0

tagElementsMultiple ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Maybe Element ->
  Text ->
  MaybeT m [Element]
tagElementsMultiple mParent etag = do
  (l, c) <- lift $ tagElements mParent etag
  if (l == 0)
  then pure []
  else forM [0..l-1] $ MaybeT . item c

tagElementsIx ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Maybe Element ->
  Text ->
  Word ->
  MaybeT m Element
tagElementsIx mParent etag i = do
  (l, c) <- lift $ tagElements mParent etag
  MaybeT $
    if (i < 0 || l <= i)
    then pure Nothing
    else item c i
