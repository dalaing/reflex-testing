{-|
Copyright   : (c) 2017, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Todo.Add (
    addItem
  ) where

import Data.Text (Text)

import Reflex.Dom.Core

addItem ::
  MonadWidget t m =>
  m (Event t Text)
addItem = mdo
  pure never

-- helper function to grab the state (focus and text)
-- helper function to focus / type / press enter
-- - down to keypress and keyrelease modelling?
-- at the higher level we want a mix of
-- - all text
-- - all text, with some whitespace at either end
-- - all whitespace
-- - purely empty
-- 50% ws 75% non-ws 50% ws
-- capturing streams of events could be interesting

-- starts with focus
-- with typing other than enter, does not fire
-- with non-empty after trim, fires with the trimmed text and clears the input
-- with empty after trim, does not fire but clears the input
