{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Counter.Bench (
    goCounter
  ) where

import Control.Monad (void, forever)
import Data.Proxy (Proxy (..))

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)

import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue

import GHCJS.DOM.Types (JSM)
import Reflex.Dom (run)

import Criterion.Main

import Reflex.Test
import Counter

testRender ::
  ReaderT (TestingEnv (Maybe Int)) JSM ()
testRender =
  void resetTest

testAdd ::
  ReaderT (TestingEnv (Maybe Int)) JSM ()
testAdd = do
  resetTest
  simulateClick "add-btn"
  waitForCondition (== (Just 1))

testClear ::
  ReaderT (TestingEnv (Maybe Int)) JSM ()
testClear = do
  resetTest
  simulateClick "add-btn"
  simulateClick "clear-btn"
  waitForCondition (== (Just 0))

data BenchToken =
    BTRender
  | BTAdd
  | BTClear
  deriving (Eq, Ord, Show)

testLoop ::
  TQueue BenchToken ->
  TMVar () ->
  IO ()
testLoop q t = run . void $ do
  env <- mkTestingEnv (readOutput' (Proxy :: Proxy Int) "count-output") counter
  flip runReaderT env $ do
    forever $ do
      bt <- liftIO . atomically $ readTQueue q
      case bt of
        BTRender -> testRender
        BTAdd -> testAdd
        BTClear -> testClear
      liftIO . atomically $ putTMVar t ()

goCounter :: IO ()
goCounter = do
  q <- liftIO . atomically $ newTQueue
  t <- liftIO . atomically $ newEmptyTMVar
  _ <- forkIO $ testLoop q t
  defaultMain $ [
    bgroup "counter" $ [
      bench "render" $ nfIO $ do
        liftIO . atomically . writeTQueue q $ BTRender
        liftIO . atomically . takeTMVar $ t
    , bench "add" $ nfIO $ do
        liftIO . atomically . writeTQueue q $ BTAdd
        liftIO . atomically . takeTMVar $ t
    , bench "clear" $ nfIO $ do
        liftIO . atomically . writeTQueue q $ BTClear
        liftIO . atomically . takeTMVar $ t
    ]
    ]
