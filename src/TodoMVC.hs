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

import Control.Monad (void)
import Data.Monoid ((<>))

import Control.Lens

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

data TodoItem =
  TodoItem {
    _tiComplete :: Bool
  , _tiText :: Text
  } deriving (Eq, Ord, Show)

makeLenses ''TodoItem

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

complete ::
  MonadWidget t m =>
  Bool ->
  Event t Bool ->
  Event t () ->
  m (Event t Bool, Event t ())
complete initial eMarkAllComplete eClearComplete = do
  cb <- checkbox initial $ def
    & checkboxConfig_setValue .~ eMarkAllComplete
    & checkboxConfig_attributes .~ pure ("class" =: "toggle")

  let
    eChange = cb ^. checkbox_change
    eRemove = gate (current $ cb ^. checkbox_value) eClearComplete

  pure (eChange, eRemove)

textRead ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t ())
textRead dText = do
  (e, _) <- el' "label" . dynText $ dText
  pure . void $ domEvent Dblclick e

remove ::
  MonadWidget t m =>
  m (Event t ())
remove =
  buttonDynAttr (pure $ "class" =: "destroy") ""

todoItemRead ::
  MonadWidget t m =>
  Event t Bool ->
  Event t () ->
  Dynamic t TodoItem ->
  m (Event t (TodoItem -> TodoItem), Event t (), Event t())
todoItemRead eMarkAllComplete eClearComplete dItem = elClass "div" "view" $ do
  (eComplete, eRemoveComplete) <- complete False eMarkAllComplete eClearComplete

  eEditStart <- textRead $ view tiText <$> dItem
  eRemoveClick <- remove

  let
    eChange = set tiComplete <$> eComplete
    eRemove = leftmost [eRemoveClick, eRemoveComplete]

  pure (eChange, eRemove, eEditStart)

todoItemWrite ::
  MonadWidget t m =>
  Dynamic t TodoItem ->
  m (Event t (TodoItem -> TodoItem), Event t (), Event t())
todoItemWrite dItem = do
  ti <- textInput $ def
    & textInputConfig_attributes .~ pure ("class" =: "edit")

  let
    eChange = set tiText <$> never
    eRemove = never
    eEditStop = never

  pure (eChange, eRemove, eEditStop)

todoItem ::
  MonadWidget t m =>
  Event t Bool ->
  Event t () ->
  Dynamic t TodoItem ->
  m (Event t (TodoItem -> TodoItem), Event t ())
todoItem eMarkAllComplete eClearComplete dItem = mdo
  let
    mkCompleted False = ""
    mkCompleted True = "completed "
    dCompleted = mkCompleted . view tiComplete <$> dItem
    mkEditing False = ""
    mkEditing True = "editing "
    dEditing = mkEditing <$> dEdit

  dEdit <- holdDyn False eEdit
  (eChange, eRemove, eEdit) <- elDynClass "li" (dCompleted <> dEditing) $ mdo

    (eChangeRead, eRemoveRead, eEditStart) <- todoItemRead eMarkAllComplete eClearComplete dItem
    (eChangeWrite, eRemoveWrite, eEditStop) <- todoItemWrite dItem

    let
      eChange = mergeWith (.) [eChangeRead, eChangeWrite]
      eRemove = leftmost [eRemoveRead, eRemoveWrite]
      eEdit = leftmost $ [True <$ eEditStart, False <$ eEditStop]

    pure (eChange, eRemove, eEdit)

  pure (eChange, eRemove)

todoList ::
  MonadWidget t m =>
  Map Int TodoItem ->
  Event t Text ->
  Event t Bool ->
  Event t () ->
  m (Dynamic t (Map Int TodoItem))
todoList initial eAdd eMarkAllComplete eClearComplete = mdo
  dCount <- count eAdd
  let
    initialCount = (+ 1) . maximum . (0 :) . Map.keys $ initial
    dKey = (+ initialCount) <$> dCount
    eItem = TodoItem False <$> eAdd

  dMap <- foldDyn ($) initial . mergeWith (.) $ [
            Map.insert <$> current dKey <@> eItem
          , Map.mergeWithKey (\_ f x -> Just (f x)) (const mempty) id <$> eChanges
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dOut <- elClass "ul" "todo-list" . list dMap $
    todoItem eMarkAllComplete eClearComplete

  let
    eChanges = switch . current . fmap (mergeMap . fmap fst) $ dOut
    eRemoves = fmap Map.keys . switch . current . fmap (mergeMap . fmap snd) $ dOut

  pure dMap

mainSection ::
  MonadWidget t m =>
  Map Int TodoItem ->
  Event t Text ->
  Event t () ->
  m (Dynamic t Int, Dynamic t Bool)
mainSection initial eAdd eClearComplete = mdo
  let
    mkClass 0 = "hidden "
    mkClass _ = ""
    sClass = "main "
    dClass = pure sClass <> mkClass <$> dSize
    initialAllComplete = all (view tiComplete) initial
  (dSize, dAny) <- elDynClass "section" dClass $ mdo
      eMarkAllComplete <- markAllComplete initialAllComplete dAll

      dMap <- todoList initial eAdd eMarkAllComplete eClearComplete

      let
        dSize = Map.size <$> dMap
        dAny = any (view tiComplete) <$> dMap
        dAll = all (view tiComplete) <$> dMap
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

todomvc :: Map Int TodoItem -> JSM ()
todomvc initial =
  let
    initialAnyComplete = any (view tiComplete) initial
  in
    mainWidgetWithHead headSection $
      elClass "section" "todoapp" $ mdo
        eAdd <- header
        (dSize, dAny) <- mainSection initial eAdd eClearComplete
        eClearComplete <- footer initialAnyComplete dSize dAny
        pure ()

todomvcExample :: JSM ()
todomvcExample =
  todomvc . Map.fromList $
    [ (1, TodoItem False "A")
    , (2, TodoItem True "B")
    , (3, TodoItem False "C")
    ]
