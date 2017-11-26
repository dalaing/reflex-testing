{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Test.Types (
    TestJSM(..)
  , TestingEnv(..)
  , teQueue
  , mkTestingEnv
  , runTestingEnv
  ) where

import Control.Lens.TH (makeClassy)

import Control.Monad.Trans (lift, MonadIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Morph (hoist)

import Control.Monad.STM (STM)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar)

import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.Types (JSM, MonadJSM(..))

import Reflex.Dom.Core (HasDocument(..))

import Hedgehog

newtype TestJSM a =
  TestJSM { unTestJSM :: JSM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadJSM)

instance MonadJSM (TestT (GenT (ReaderT r TestJSM))) where
  liftJSM' = lift . lift . lift . liftJSM'

instance MonadJSM (TestT (GenT TestJSM)) where
  liftJSM' = lift . lift . liftJSM'

instance HasDocument TestJSM where
  askDocument = TestJSM currentDocumentUnchecked

instance HasDocument (TestT (GenT TestJSM)) where
  askDocument = lift $ lift $ askDocument

instance HasDocument (TestT (GenT (ReaderT r TestJSM))) where
  askDocument = lift $ lift $ lift $ askDocument

data TestingEnv a =
  TestingEnv {
    _teQueue    :: TMVar a
  }

makeClassy ''TestingEnv

mkTestingEnv :: STM (TestingEnv a)
mkTestingEnv =
  TestingEnv <$> newEmptyTMVar

runTestingEnv ::
  TestingEnv e ->
  TestT (GenT (ReaderT (TestingEnv e) TestJSM)) a ->
  TestT (GenT JSM) a
runTestingEnv env =
  hoist $ hoist $ unTestJSM . flip runReaderT env
