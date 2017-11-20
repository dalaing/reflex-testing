{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Counter.Test (
    counterStateMachine
  ) where

import Data.Proxy (Proxy(..))

import Control.Monad.Trans (lift, MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Morph (hoist)

import Control.Monad.STM (atomically)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Internal.Property

import GHCJS.DOM.Types (JSM, MonadJSM, liftJSM, askJSM)

import Reflex.Dom.Core (mainWidget)
import Reflex.Dom (run)

import Reflex.Test
import Counter

data ModelState (v :: * -> *) =
  ModelState Int
  deriving (Eq, Ord, Show)

data Add (v :: * -> *) = Add
  deriving (Eq, Show)

instance HTraversable Add where
  htraverse _ Add = pure Add

data Clear (v :: * -> *) = Clear
  deriving (Eq, Show)

instance HTraversable Clear where
  htraverse _ Clear = pure Clear

s_add ::
  ( Monad n
  , MonadTest m
  , MonadReader (TestingEnv Int) m
  , MonadJSM m
  ) =>
  Command n m ModelState
s_add =
  let
    gen _ =
      Just $ pure Add
    execute Add = do
      _ <- simulateClick "add-btn"
      waitForRender
  in
    Command gen execute [
        Update $ \(ModelState c) Add _o ->
          ModelState (c + 1)
      , Ensure $ \_before (ModelState c) Add b -> do
         c === b
    ]

s_clear ::
  ( Monad n
  , MonadTest m
  , MonadReader (TestingEnv Int) m
  , MonadJSM m
  ) =>
  Command n m ModelState
s_clear =
  let
    gen _ =
      Just $ pure Clear
    execute Clear = do
      _ <- simulateClick "clear-btn"
      waitForRender
  in
    Command gen execute [
        Update $ \_s Clear _o ->
          ModelState 0
      , Ensure $ \_before (ModelState c) Clear b -> do
          0 === b
          0 === c
    ]

initialState ::
  ModelState v
initialState =
  ModelState 0

counterStateMachine ::
  PropertyT JSM ()
counterStateMachine = do
  env <- liftIO . atomically $ mkTestingEnv
  _ <- lift $ do
    mainWidget $ testingWidget (readOutput' (Proxy :: Proxy Int) "count-output") env $ counter
    runReaderT resetTest env

  let
    hoistEnv = hoistCommand (hoist $ hoist $ unTestJSM . flip runReaderT env)

  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialResettableState [
      hoistEnv $ s_reset initialState
    , hoistEnv $ prismCommand _Running s_add
    , hoistEnv $ prismCommand _Running s_clear
    ]
  PropertyT $ executeSequential initialResettableState actions

