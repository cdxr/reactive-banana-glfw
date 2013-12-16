{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Reactive.Banana.GLFW.Button
(
    -- * Input Sources
    keyEvent,
    mouseEvent,

    -- ** Predicates
    match,
    matchCode,

    ButtonState(..),
    Press(..),
    press,
    release,
    repeating,
    mods,
)
where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Reactive.Banana.GLFW.Types

import qualified Reactive.Banana.GLFW.WindowHandler as WH


-- * Events

keyEvent :: (Frameworks t) => WH.WindowHandler -> Moment t (Event t KeyEvent)
keyEvent = WH.fromAddHandler' . WH.keyEvent

mouseEvent :: (Frameworks t) => WH.WindowHandler -> Moment t (Event t MouseEvent)
mouseEvent = WH.fromAddHandler' . WH.mouseEvent


-- * Filters


match :: Key -> KeyEvent -> Bool
match k = (== k) . key

matchCode :: Int -> KeyEvent -> Bool
matchCode sc = (== SC sc) . scancode


-- | @mods ms a@ is True iff for all @m@ in @ms@, @modkey m a@.
--
-- @mods [] a@ is True only when @a@ includes no ModKeys.
mods :: [ModKey] -> ButtonEvent b s -> Bool
mods ms be = all (\m -> elem m bms == elem m ms) enumerateModKeys
  where
    bms = modifiers be


data Press = Release | Press
    deriving (Show, Eq, Ord, Bounded, Enum)

-- | Class of types that might represent the pressing or releasing of a `Button`.
class ButtonState b where
    getPress :: b -> Maybe Press

press :: (ButtonState s) => ButtonEvent b s -> Bool
press = (== Just Press) . getPress . buttonState

release :: (ButtonState s) => ButtonEvent b s -> Bool
release = (== Just Release) . getPress . buttonState

repeating :: ButtonEvent b KeyState -> Bool
repeating be = case buttonState be of
    KeyState'Repeating -> True
    _                  -> False


instance ButtonState KeyState where
    getPress s = case s of
        KeyState'Pressed   -> Just Press
        KeyState'Released  -> Just Release
        KeyState'Repeating -> Nothing

instance ButtonState MouseButtonState where
    getPress s = Just $ case s of
        MouseButtonState'Pressed  -> Press
        MouseButtonState'Released -> Release
