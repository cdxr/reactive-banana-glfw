{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Reactive.Banana.GLFW.Button
(
    -- * Input Sources
    keyEvent,
    mouseEvent,
    InputEvent(..),

    -- ** State Predicates
    ButtonState(..),
    Press(..),
    press,
    release,
    repeating,
    mods,
    Match(..),
    button,
)
where

import Data.Maybe ( isNothing )

import Reactive.Banana

import Reactive.Banana.GLFW.Types
import Reactive.Banana.GLFW.Internal.WindowE


class InputEvent e where
    inputEvent :: WindowE t -> Event t e

instance InputEvent KeyEvent where
    inputEvent = keyEvent

instance InputEvent MouseEvent where
    inputEvent = mouseEvent


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


-- | @mods ms a@ is True iff for all @m@ in @ms@, @modkey m a@.
--
-- @mods [] a@ is True only when @a@ includes no ModKeys.
mods :: [ModKey] -> ButtonEvent b s -> Bool
mods ms be = all (\m -> elem m bms == elem m ms) enumerateModKeys
  where
    bms = modifiers be


class Match m e | m -> e where
    match :: m -> e -> Bool

instance Match Key KeyEvent where
    match k = (==) k . key

instance Match ScanCode KeyEvent where
    match sc = (==) sc . scancode

instance Match MouseButton MouseEvent where
    match mb = (==) mb . mouseButton


button :: (InputEvent e, Match m e) => WindowE t -> m -> Event t e
button w m = filterE (match m) (inputEvent w)
