{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Test.Hedgehog (
    hoistCommand
  , hoistAction
  , hoistSequential
  , sequentialResettable
  , runSequentialResettable
  , ResettableState
  , initialResettableState
  , _Setup
  , _Running
  , s_reset
  , prismCommand
  , propertyJSM
  ) where

import Control.Monad (void)
import Data.Maybe (isJust)
import Data.Typeable (Typeable)

import Control.Lens

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Morph (hoist)

import Reflex.Dom.Core (HasDocument(..))
import Reflex.Dom (run)

import GHCJS.DOM.Types (MonadJSM, JSM, askJSM)
import Language.Javascript.JSaddle.Monad (runJSaddle)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Internal.Property
import Hedgehog.Internal.State

import Reflex.Test

hoistCommand ::
  (forall x. m x -> m' x) ->
  Command n m s ->
  Command n m' s
hoistCommand f (Command gen execute callbacks) =
  Command gen (f . execute) callbacks

hoistAction ::
  (forall x. m x -> m' x) ->
  Action m s ->
  Action m' s
hoistAction f (Action aInput aOutput aExecute aRequire aUpdate aEnsure) =
  Action aInput aOutput (f . aExecute) aRequire aUpdate aEnsure

hoistSequential ::
  (forall x. m x -> m' x) ->
  Sequential m s ->
  Sequential m' s
hoistSequential f (Sequential xs) =
  Sequential (fmap (hoistAction f) xs)

prismCallback ::
  (forall v. Prism' (s v) (t v)) ->
  Callback i o t ->
  Callback i o s
prismCallback p (Require f) =
  Require $ \st i ->
    case preview p st of
      Just st' -> f st' i
      Nothing -> False
prismCallback p (Update f) =
  Update $ \st i v ->
    case preview p st of
      Just st' -> review p $ f st' i v
      Nothing -> st
prismCallback p (Ensure f) =
  Ensure $ \stb sta i o ->
    case (preview p stb, preview p sta) of
      (Just stb' , Just sta') -> f stb' sta' i o
      _ -> pure ()

prismCommand ::
  (forall v. Prism' (s v) (t v)) ->
  Command n m t ->
  Command n m s
prismCommand p (Command gen execute callbacks) =
  let
    gen' st = case preview p st of
      Just st' -> gen st'
      Nothing -> Nothing
    callbacks' = Require (\st _ -> isJust $ preview p st) : fmap (prismCallback p) callbacks
  in
    Command
      gen'
      execute
      callbacks'

data ResettableState st (v :: * -> *) =
    Setup
  | Running (st v)
  deriving (Eq, Ord, Show)

makePrisms ''ResettableState

initialResettableState :: ResettableState st v
initialResettableState = Setup

data Reset (v :: * -> *) = Reset
  deriving (Eq, Show)

instance HTraversable Reset where
  htraverse _ Reset = pure Reset

s_reset ::
  ( Monad n
  , MonadTest m
  , MonadReader (TestingEnv a) m
  , MonadJSM m
  , HasDocument m
  , Typeable a
  , Show (st Concrete)
  , Eq (st Concrete)
  ) =>
  (forall v. st v) ->
  Command n m (ResettableState st)
s_reset initial =
  let
    gen st =
      case preview _Setup st of
        Just _ -> Just $ pure Reset
        _ -> Nothing
    execute Reset =
      resetTest
  in
    Command gen execute [
        Require $ \s Reset ->
            isJust (preview _Setup s)
      , Update $ \_s Reset _o ->
          review _Running initial
      , Ensure $ \_before after Reset _b ->
          review _Running initial === after
    ]

sequentialResettable ::
  ( MonadGen n
  , MonadTest m
  , MonadReader (TestingEnv e) m
  , MonadJSM m
  , HasDocument m
  , Typeable e
  , Eq (state Concrete)
  , Show (state Concrete)
  ) =>
  Range.Range Int ->
  (forall v. state v) ->
  [Command n m state] ->
  n (Sequential m (ResettableState state))
sequentialResettable range initial commands = do
  Gen.sequential range initialResettableState $
    s_reset initial : fmap (prismCommand _Running) commands

runSequentialResettable ::
  ( Typeable e
  , Eq (state Concrete)
  , Show (state Concrete)
  ) =>
  TestingEnv e ->
  Range.Range Int ->
  (forall v. state v) ->
  [Command (GenT Identity) (TestT (GenT (ReaderT (TestingEnv e) TestJSM))) state] ->
  PropertyT JSM ()
runSequentialResettable env range initial commands = do
  actions <- forAll $ do
    s <- sequentialResettable range initial commands
    pure $ hoistSequential (runTestingEnv env) s
  PropertyT $ executeSequential initialResettableState actions

propertyJSM ::
  PropertyT JSM () ->
  IO ()
propertyJSM p = run . void $ do
  ctx <- askJSM
  liftIO .
    check .
    property .
    hoist (runJSaddle ctx) $
    p
