{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
module Counter.Test (
    counterStateMachine
  ) where

import Data.Proxy (Proxy(..))

import Control.Monad.Trans (lift)
import Control.Monad.Reader (runReaderT)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Internal.Property

import GHCJS.DOM.Types (JSM)

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
  ) =>
  TestingEnv (Maybe Int) ->
  Command n (TestT (GenT JSM)) ModelState
s_reset env =
  let
    gen st =
      case st of
        Setup -> Just $ pure Reset
        _ -> Nothing
    execute Reset = lift . lift . flip runReaderT env $ do
      _ <- simulateClick "reset-btn"
      waitForRender
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
  ) =>
  TestingEnv (Maybe Int) ->
  Command n (TestT (GenT JSM)) ModelState
s_add env =
  let
    gen st =
      case st of
        Running _ -> Just $ pure Add
        _ -> Nothing
    execute Add = lift . lift . flip runReaderT env $ do
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
  ) =>
  TestingEnv (Maybe Int) ->
  Command n (TestT (GenT JSM)) ModelState
s_clear env =
  let
    gen st =
      case st of
        Running _ -> Just $ pure Clear
        _ -> Nothing
    execute Clear = lift . lift . flip runReaderT env $ do
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
  env <- lift $ mkTestingEnv (readOutput' (Proxy :: Proxy Int) "count-output") (testWidget counter)
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialState [
      s_reset env
    , s_add env
    , s_clear env
    ]
  PropertyT $ executeSequential initialState actions
