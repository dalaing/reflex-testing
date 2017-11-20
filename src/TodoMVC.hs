{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module TodoMVC (
    todomvc
  ) where

import Control.Lens
import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core

import GHCJS.DOM.Types (JSM)

import Reflex.Dom (run)

stylesheet ::
  MonadWidget t m =>
  Text ->
  m ()
stylesheet s =
  elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", s)]) $
    pure ()

headSection ::
  MonadWidget t m =>
  m ()
headSection = do
  elAttr "meta" ("charset" =: "utf-8") $
    pure ()
  stylesheet "css/index.css"

buttonDynAttr ::
  MonadWidget t m =>
  Dynamic t (Map Text Text) ->
  Text ->
  m (Event t ())
buttonDynAttr dAttrs label = do
  (e, _) <- elDynAttr' "button" dAttrs $ text label
  pure $ domEvent Click e

data HeaderState (v :: * -> *) =
  HeaderState
  deriving (Eq, Ord, Show)

addItem ::
  MonadWidget t m =>
  m (Event t Text)
addItem = mdo
  ti <- textInput $ def
    & textInputConfig_attributes .~ pure ("class" =: "new-todo" <>
                                          "autofocus" =: "" <>
                                          "placeholder" =: "What needs to be done?"
                                         )
    & textInputConfig_setValue .~ ("" <$ eEnter)

  let
    bValue = current . fmap (Text.strip) $ ti ^. textInput_value
    isKey k = (== k) . keyCodeLookup . fromIntegral
    eKey = ti ^. textInput_keypress
    eEnter = ffilter (isKey Enter) eKey
    eAtEnter = Text.strip <$> bValue <@ eEnter
    eDone = ffilter (not . Text.null) eAtEnter

  pure eDone

header ::
  MonadWidget t m =>
  m (Event t Text)
header =
  elClass "header" "header" $ do
    el "h1" $
      text "todos"
    addItem

markAllComplete ::
  MonadWidget t m =>
  Bool ->
  Dynamic t Bool ->
  m (Event t Bool)
markAllComplete initialAllComplete dAll = do
  cb <- checkbox initialAllComplete $ def
    & checkboxConfig_setValue .~ updated dAll
    & checkboxConfig_attributes .~ pure ("id" =: "toggle-all" <> "class" =: "toggle-all")
  elAttr "label" ("for" =: "toggle-all") $ text "Mark all as complete"
  pure never

data MainState (v :: * -> *) =
  MainState
  deriving (Eq, Ord, Show)

mainSection ::
  MonadWidget t m =>
  Bool ->
  Event t () ->
  m (Dynamic t Int, Dynamic t Bool)
mainSection initialAllComplete eClearComplete = mdo
  let
    mkClass 0 = "hidden "
    mkClass _ = ""
    sClass = "main "
    dClass = pure sClass <> mkClass <$> dSize
  (dSize, dAny) <- elDynClass "section" dClass $ mdo
      eMarkAllComplete <- markAllComplete initialAllComplete dAll

      dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
              ]

      dOut <- elClass "ul" "todo-list" . listWithKey dMap $ \k dv -> do
        pure ()


      let
        dSize = pure 2
        dAny = pure True
        dAll = pure False
      pure (dSize, dAny)
  pure (dSize, dAny)

todoCount ::
  MonadWidget t m =>
  Dynamic t Int ->
  m ()
todoCount dSize =
  let
    pluralize 1 = "item"
    pluralize _ = "items"
  in
    elClass "span" "todo-count" $ do
      el "strong" $ display dSize
      text " "
      dynText $ pluralize <$> dSize
      text " left"

clearComplete ::
  MonadWidget t m =>
  Bool ->
  Dynamic t Bool ->
  m (Event t ())
clearComplete initial dAny =
  let
    mkClass False = "hidden "
    mkClass True = ""
    sClass = "clear-completed "
    dClass = pure sClass <> mkClass <$> dAny
    dAttrs = ("class" =:) <$> dClass
  in
    buttonDynAttr dAttrs "Clear completed"

data FooterState (v :: * -> *) =
  FooterState
  deriving (Eq, Ord, Show)

footer ::
  MonadWidget t m =>
  Bool ->
  Dynamic t Int ->
  Dynamic t Bool ->
  m (Event t ())
footer initialAnyComplete dSize dAny =
  let
    mkClass 0 = "hidden "
    mkClass _ = ""
    sClass = "footer "
    dClass = pure sClass <> mkClass <$> dSize
  in
    elDynClass "footer" dClass $ do
      todoCount dSize
      clearComplete initialAnyComplete dAny

todomvc :: JSM ()
todomvc =
  mainWidgetWithHead headSection $
    elClass "section" "todoapp" $ mdo
      eAdd <- header
      (dSize, dAny) <- mainSection False eClearComplete
      eClearComplete <- footer False dSize dAny
      pure ()
