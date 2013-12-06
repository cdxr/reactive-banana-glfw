-- | This module exports types for GLFW input events. `GLFW.Key` and
-- `GLFW.MouseButton` are re-exported from the GLFW-b package.
--
module Reactive.Banana.GLFW.Types
(
    -- * Event types
    MouseClick(..),
    KeyPress(..),
    -- * Button identifiers
    GLFW.MouseButton(..),
    ScanCode(..),
    ModKey(..),
    GLFW.Key(..),
)
where

import Graphics.UI.GLFW as GLFW


-- | A state-change for a single key.
--
-- A particular key can be watched by pattern matching on its `Key` or
-- `ScanCode`. It is a bad idea to pattern match on both because the
-- scancode-key mapping can vary between machines.
--
data KeyPress = KeyPress !Key !ScanCode !KeyState ![ModKey]
    deriving (Show, Eq, Ord)


-- | A state-change for a single mouse button.
data MouseClick = MouseClick !MouseButton !MouseButtonState ![ModKey]
    deriving (Show, Eq, Ord)


-- | The scancode identifying a particular key.
--
-- The scancode of a key is machine-specific. They are intended to allow users
-- to bind keys that do not have a GLFW `Key` token.
newtype ScanCode = SC Int
    deriving (Show, Read, Eq, Ord)


-- | A modifier key. A list of modifier keys is reported for every key press
-- and mouse click.
data ModKey = Shift | Ctl | Alt | Super
    deriving (Show, Read, Ord, Eq, Bounded, Enum)
