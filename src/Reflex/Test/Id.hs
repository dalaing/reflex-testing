{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Reflex.Test.Id (
    idElement
  ) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Text (Text)

import Reflex.Dom.Core (HasDocument(..))

import GHCJS.DOM.NonElementParentNode (getElementById)
import GHCJS.DOM.Element (Element)
import GHCJS.DOM.Types (MonadJSM)

idElement ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Text ->
  MaybeT m Element
idElement eid = do
  doc <- lift askDocument
  MaybeT . getElementById doc $ eid
