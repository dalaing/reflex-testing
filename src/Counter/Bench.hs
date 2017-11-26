{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Counter.Bench (
    goCounter
  ) where

import Control.Monad (void, forever)
import Data.Proxy (Proxy (..))

import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)

import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue

import GHCJS.DOM.Types (JSM, MonadJSM)
import Reflex.Dom.Core (mainWidget, HasDocument)
import Reflex.Dom (run)

import Criterion.Main

import Reflex.Test
import Reflex.Test.Maybe
import Reflex.Test.Id
import Reflex.Test.Button
import Reflex.Test.Text

import Counter

testRender ::
  ( MonadReader (TestingEnv Int) m
  , MonadJSM m
  , HasDocument m
  ) =>
  m ()
testRender =
  pure ()

testAdd ::
  ( MonadReader (TestingEnv Int) m
  , MonadJSM m
  , HasDocument m
  ) =>
  m ()
testAdd = do
  void . checkMaybe $ idElement "add-btn" >>= clickButton
  waitForCondition (== 1)

testClear ::
  ( MonadReader (TestingEnv Int) m
  , MonadJSM m
  , HasDocument m
  ) =>
  m ()
testClear = do
  void . checkMaybe $ do
    idElement "add-btn" >>= clickButton
    idElement "clear-btn" >>= clickButton
  waitForCondition (== 0)

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
  env <- liftIO . atomically $ mkTestingEnv
  _ <- mainWidget $ testingWidget (idElement "count-output" >>= readText (Proxy :: Proxy Int)) env $ counter
  unTestJSM . flip runReaderT env $ do
    forever $ do
      resetTest
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
