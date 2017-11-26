{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Reflex.Test.Maybe (
    checkMaybe
  ) where

import Data.Maybe (isJust)

import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

checkMaybe ::
  Monad m =>
  MaybeT m a ->
  m Bool
checkMaybe =
  fmap isJust .
  runMaybeT
