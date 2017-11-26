{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Reflex.Test.Button (
    clickButton
  ) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))

import GHCJS.DOM.Element (Element(..))
import GHCJS.DOM.HTMLElement (HTMLElement(..), click)
import GHCJS.DOM.Types (MonadJSM, castTo)

clickButton ::
  MonadJSM m =>
  Element ->
  MaybeT m ()
clickButton e = do
  he <- MaybeT $ castTo HTMLElement e
  lift $ click he
