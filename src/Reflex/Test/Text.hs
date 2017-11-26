{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Reflex.Test.Text (
    getText
  , readText
  , readText'
  ) where

import Text.Read (readMaybe)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Text (Text)
import qualified Data.Text as Text

import GHCJS.DOM.Node (getTextContent, IsNode)
import GHCJS.DOM.Types (MonadJSM)

getText ::
  ( IsNode n
  , MonadJSM m
  ) =>
  n ->
  MaybeT m Text
getText =
  MaybeT . getTextContent

readText ::
  ( Read a
  , IsNode n
  , MonadJSM m
  ) =>
  proxy a ->
  n ->
  MaybeT m a
readText _ =
  readText'

readText' ::
  ( Read a
  , IsNode n
  , MonadJSM m
  ) =>
  n ->
  MaybeT m a
readText' n = do
  t <- MaybeT . getTextContent $ n
  MaybeT . pure . readMaybe . Text.unpack $ t
