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


data KeyPress = KeyPress !Key !ScanCode !KeyState ![ModKey]
    deriving (Show, Eq, Ord)

data MouseClick = MouseClick !MouseButton !MouseButtonState ![ModKey]
    deriving (Show, Eq, Ord)


newtype ScanCode = SC Int
    deriving (Show, Read, Eq, Ord)


data ModKey = Shift | Ctl | Alt | Super
    deriving (Show, Read, Ord, Eq, Bounded, Enum)
