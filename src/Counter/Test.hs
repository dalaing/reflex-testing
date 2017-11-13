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

import Control.Monad.Trans (lift, MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Morph (hoist)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Internal.Property

import GHCJS.DOM.Types (JSM, MonadJSM, liftJSM, askJSM)

import Reflex.Test
import Counter

data ModelState (v :: * -> *) =
    Setup
  | Running Int
  deriving (Eq, Ord, Show)

data Reset (v :: * -> *) = Reset
  deriving (Eq, Show)

instance HTraversable Reset where
  htraverse _ Reset = pure Reset

data Add (v :: * -> *) = Add
  deriving (Eq, Show)

instance HTraversable Add where
  htraverse _ Add = pure Add

data Clear (v :: * -> *) = Clear
  deriving (Eq, Show)

instance HTraversable Clear where
  htraverse _ Clear = pure Clear

s_reset ::
  ( Monad n
  , MonadTest m
  , MonadReader (TestingEnv (Maybe Int)) m
  , MonadJSM m
  ) =>
  Command n m ModelState
s_reset =
  let
    gen st =
      case st of
        Setup -> Just $ pure Reset
        _ -> Nothing
    execute Reset =
      resetTest
  in
    Command gen execute [
        Update $ \_s Reset _o ->
            Running 0
      , Ensure $ \_before after Reset b -> do
          Running 0 === after
          Just 0 === b
    ]

s_add ::
  ( Monad n
  , MonadTest m
  , MonadReader (TestingEnv (Maybe Int)) m
  , MonadJSM m
  ) =>
  Command n m ModelState
s_add =
  let
    gen st =
      case st of
        Running _ -> Just $ pure Add
        _ -> Nothing
    execute Add = do
      _ <- simulateClick "add-btn"
      waitForRender
  in
    Command gen execute [
        Require $ \s Add ->
            case s of
              Running _ -> True
              _ -> False
      , Update $ \s Add _o ->
          case s of
            Running c -> Running (c + 1)
            Setup -> Setup
      , Ensure $ \_before after Add b -> do
          case after of
            Running c -> do
              Just c === b
            _ -> pure ()
    ]

s_clear ::
  ( Monad n
  , MonadTest m
  , MonadReader (TestingEnv (Maybe Int)) m
  , MonadJSM m
  ) =>
  Command n m ModelState
s_clear =
  let
    gen st =
      case st of
        Running _ -> Just $ pure Clear
        _ -> Nothing
    execute Clear = do
      _ <- simulateClick "clear-btn"
      waitForRender
  in
    Command gen execute [
        Require $ \s Clear ->
            case s of
              Running _ -> True
              _ -> False
      , Update $ \s Clear _o ->
          case s of
            Running _ -> Running 0
            Setup -> Setup
      , Ensure $ \_before after Clear b -> do
          case after of
            Running c -> do
              Just 0 === b
              0 === c
            _ -> pure ()
    ]

initialState ::
  ModelState v
initialState =
  Setup

counterStateMachine ::
  PropertyT JSM ()
counterStateMachine = do
  env <- lift $ mkTestingEnv (readOutput' (Proxy :: Proxy Int) "count-output") counter
  let hoistEnv = hoistCommand (hoist $ hoist $ unTestJSM . flip runReaderT env)
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialState [
      hoistEnv s_reset
    , hoistEnv s_add
    , hoistEnv s_clear
    ]
  PropertyT $ executeSequential initialState actions
