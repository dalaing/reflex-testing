{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Reflex.Test.Checkbox (
    getChecked
  , setChecked
  , toggleChecked
  ) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))

import GHCJS.DOM.Element (Element(..))
import qualified GHCJS.DOM.HTMLElement as HE (HTMLElement(..), click)
import qualified GHCJS.DOM.HTMLInputElement as HIE (HTMLInputElement(..), getChecked, setChecked)
import GHCJS.DOM.Types (MonadJSM, castTo)

getChecked ::
  MonadJSM m =>
  Element ->
  MaybeT m Bool
getChecked e = do
  hie <- MaybeT $ castTo HIE.HTMLInputElement e
  lift $ HIE.getChecked hie

setChecked ::
  MonadJSM m =>
  Element ->
  Bool ->
  MaybeT m ()
setChecked e b = do
  he <- MaybeT $ castTo HE.HTMLElement e
  lift $ HE.click he
  hie <- MaybeT $ castTo HIE.HTMLInputElement e
  lift $ HIE.setChecked hie b

toggleChecked ::
  MonadJSM m =>
  Element ->
  MaybeT m ()
toggleChecked e = do
  b <- getChecked e
  setChecked e (not b)
