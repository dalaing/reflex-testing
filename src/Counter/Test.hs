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

import Control.Monad (void)
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

import Reflex.Dom.Core (mainWidget, HasDocument)
import Reflex.Dom (run)

import Reflex.Test
import Reflex.Test.Maybe
import Reflex.Test.Id
import Reflex.Test.Button
import Reflex.Test.Text
import Reflex.Test.Hedgehog

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
  , HasDocument m
  ) =>
  Command n m ModelState
s_add =
  let
    gen _ =
      Just $ pure Add
    execute Add = do
      void . checkMaybe $ idElement "add-btn" >>= clickButton
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
  , HasDocument m
  ) =>
  Command n m ModelState
s_clear =
  let
    gen _ =
      Just $ pure Clear
    execute Clear = do
      void . checkMaybe $ idElement "clear-btn" >>= clickButton
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
  env <- lift $ setupResettableWidget (idElement "count-output" >>= readText (Proxy :: Proxy Int)) counter
  runSequentialResettable env (Range.linear 1 100) initialState [
      s_add
    , s_clear
    ]
