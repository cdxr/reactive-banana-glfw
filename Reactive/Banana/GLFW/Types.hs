-- | This module exports types for GLFW input events. `GLFW.Key` and
-- `GLFW.MouseButton` are re-exported from the GLFW-b package.
--
module Reactive.Banana.GLFW.Types
(
    -- * Event types
    ButtonEvent(..),
    ModKey(..),
    enumerateModKeys,

    -- ** KeyEvent
    KeyEvent,
    key,
    scancode,

    -- ** MouseEvent
    MouseEvent,
    mouseButton,

    -- * Button identifiers
    GLFW.MouseButton(..),
    ScanCode(..),
    GLFW.Key(..),

    -- * Button state
    GLFW.MouseButtonState(..),
    GLFW.KeyState(..),
) where

import Graphics.UI.GLFW as GLFW


-- | A change in the state @s@ of a button identified by @b@, annotated
-- with a list of modifier keys.
data ButtonEvent b s = ButtonEvent
    { buttonId    :: !b
    , buttonState :: !s
    , modifiers   :: ![ModKey]
    } deriving (Show, Eq, Ord)


-- | A modifier key. A list of modifier keys is reported for every key press
-- and mouse click.
data ModKey = Shift | Ctl | Alt | Super
    deriving (Show, Read, Ord, Eq, Bounded, Enum)

-- | A list of all modifier keys
enumerateModKeys :: [ModKey]
enumerateModKeys = [Shift .. Super]


-- | A state-change for a single key.
--
-- A particular key can be filtered by pattern matching on its `Key` or
-- `ScanCode`. It is a bad idea to pattern match on both because the
-- scancode-key mapping can vary between machines.
type KeyEvent = ButtonEvent (Key, ScanCode) KeyState


-- | The scancode identifying a particular key.
--
-- The scancode of a key is machine-specific. They are intended to allow users
-- to bind keys that do not have a GLFW `Key` token.
newtype ScanCode = SC Int
    deriving (Show, Read, Eq, Ord)


key :: KeyEvent -> Key
key = fst . buttonId

scancode :: KeyEvent -> ScanCode
scancode = snd . buttonId


-- | A state-change for a single mouse button.
type MouseEvent = ButtonEvent MouseButton MouseButtonState

mouseButton :: MouseEvent -> MouseButton
mouseButton = buttonId
