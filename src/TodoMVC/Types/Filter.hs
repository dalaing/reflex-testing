{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module TodoMVC.Types.Filter (
    Filter(..)
  , filterShowComplete
  ) where

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

