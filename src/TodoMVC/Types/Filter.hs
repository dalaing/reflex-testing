{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.Types.Filter (
    Filter(..)
  , filterShowComplete
  , filterLabel
  , parseFilterLabel
  , filterLink
  ) where

import Data.Text (Text)

data Filter =
    FAll
  | FActive
  | FComplete
  deriving (Eq, Ord, Show, Read)

filterShowComplete ::
  Filter ->
  Bool ->
  Bool
filterShowComplete FAll =
  const True
filterShowComplete FActive =
  not
filterShowComplete FComplete =
  id

filterLabel ::
  Filter ->
  Text
filterLabel FAll =
  "All"
filterLabel FActive =
  "Active"
filterLabel FComplete =
  "Complete"

parseFilterLabel ::
  Text ->
  Maybe Filter
parseFilterLabel "All" =
  Just FAll
parseFilterLabel "Active" =
  Just FActive
parseFilterLabel "Complete" =
  Just FComplete
parseFilterLabel _ =
  Nothing

filterLink ::
  Filter ->
  Text
filterLink FAll =
  "#/"
filterLink FActive =
  "#/active"
filterLink FComplete =
  "#/completed"
