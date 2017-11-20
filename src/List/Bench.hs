{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module List.Bench (
    goList
  ) where

import Control.Monad (void, forever)
import Data.Proxy (Proxy (..))

import Control.Lens

import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)

import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue

import qualified Data.Text as Text
import qualified Data.Sequence as Seq

import GHCJS.DOM.Types (JSM)
import Reflex.Dom.Core (mainWidget)
import Reflex.Dom (run)

import Criterion.Main

import Reflex.Test
import List
import List.Common

testRender ::
  ReaderT (TestingEnv (ModelState v)) JSM ()
testRender =
  pure ()

testType ::
  ReaderT (TestingEnv (ModelState v)) JSM ()
testType = do
  focusText
  typeText "test"
  blurText
  waitForCondition (\st -> not . Text.null $ st ^. msText)

testAdd ::
  ReaderT (TestingEnv (ModelState v)) JSM ()
testAdd = do
  focusText
  typeText "test"
  blurText
  void $ waitForRender
  clickAdd
  waitForCondition (\st -> st ^. msItems . to Seq.length == 1)

testRemove ::
  ReaderT (TestingEnv (ModelState v)) JSM ()
testRemove = do
  focusText
  typeText "test"
  blurText
  void $ waitForRender
  clickAdd
  void $ waitForRender
  clickRemove 0
  waitForCondition (\st -> st ^. msItems . to Seq.length == 0)

data BenchToken =
    BTRender
  | BTType
  | BTAdd
  | BTRemove
  deriving (Eq, Ord, Show)

testLoop ::
  TQueue BenchToken ->
  TMVar () ->
  IO ()
testLoop q t = run . void $ do
  env <- liftIO . atomically $ mkTestingEnv
  _ <- mainWidget $ testingWidget fetchState env $ listWidget
  flip runReaderT env $ do
    forever $ do
      resetTest
      bt <- liftIO . atomically $ readTQueue q
      case bt of
        BTRender -> testRender
        BTType -> testType
        BTAdd -> testAdd
        BTRemove -> testRemove
      liftIO . atomically $ putTMVar t ()

goList :: IO ()
goList = do
  q <- liftIO . atomically $ newTQueue
  t <- liftIO . atomically $ newEmptyTMVar
  _ <- forkIO $ testLoop q t
  defaultMain $ [
    bgroup "list" $ [
      bench "render" $ nfIO $ do
        liftIO . atomically . writeTQueue q $ BTRender
        liftIO . atomically . takeTMVar $ t
    , bench "type" $ nfIO $ do
        liftIO . atomically . writeTQueue q $ BTType
        liftIO . atomically . takeTMVar $ t
    , bench "add" $ nfIO $ do
        liftIO . atomically . writeTQueue q $ BTAdd
        liftIO . atomically . takeTMVar $ t
    , bench "remove" $ nfIO $ do
        liftIO . atomically . writeTQueue q $ BTRemove
        liftIO . atomically . takeTMVar $ t
    ]
    ]
