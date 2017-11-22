{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module TodoMVC.Filter (
    Filter(..)
  , filterShowComplete
  , filtersWidget
  ) where

import Data.Monoid ((<>))

import Data.Text (Text)

import Reflex.Dom.Core

data Filter =
    FAll
  | FActive
  | FComplete
  deriving (Eq, Ord, Show)

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

filterLink ::
  Filter ->
  Text
filterLink FAll =
  "#/"
filterLink FActive =
  "#/active"
filterLink FComplete =
  "#/completed"

filterWidget ::
  MonadWidget t m =>
  Filter ->
  Dynamic t Filter ->
  m (Event t Filter)
filterWidget f dFilter = el "li" $ do
  let
    mkClass False = ""
    mkClass True = "selected"
    dAttr = ("class" =:) . mkClass . (== f) <$> dFilter
    sAttr = "href" =: filterLink f

  (e, _) <- elDynAttr' "a" (pure sAttr <> dAttr) $
    text $ filterLabel f
  pure $ f <$ domEvent Click e

filtersWidget ::
  MonadWidget t m =>
  m (Dynamic t Filter)
filtersWidget = elClass "ul" "filters" $ mdo
  eAll <- filterWidget FAll dFilter
  eActive <- filterWidget FActive dFilter
  eComplete <- filterWidget FComplete dFilter
  dFilter <- holdDyn FAll . leftmost $ [eAll, eActive, eComplete]
  pure dFilter
