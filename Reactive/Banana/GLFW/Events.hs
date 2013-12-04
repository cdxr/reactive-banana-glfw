{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Reactive.Banana.GLFW.Events
(
    -- * Press
    Press(..),
    Pressable(..),
    press,
    release,
    -- * Button
    Button(..),
)
where


import Reactive.Banana
import Graphics.UI.GLFW

import Reactive.Banana.GLFW.Window


data Press = Release | Press
    deriving (Show, Eq, Ord, Bounded, Enum)


-- | Class of types that represent the pressing or releasing of a `Button`.
class Pressable b where
    toPress :: b -> Maybe Press

press :: (Pressable a) => a -> Bool
press = (== Just Press) . toPress

release :: (Pressable a) => a -> Bool
release = (== Just Release) . toPress


instance Pressable Press where
    toPress = Just

instance Pressable KeyState where
    toPress ks = case ks of
        KeyState'Pressed   -> Just Press
        KeyState'Released  -> Just Release
        KeyState'Repeating -> Nothing

instance Pressable KeyPress where
    toPress (KeyPress _ _ s _) = toPress s

instance Pressable MouseButtonState where
    toPress mbs = Just $ case mbs of
        MouseButtonState'Pressed  -> Press
        MouseButtonState'Released -> Release

instance Pressable MouseClick where
    toPress (MouseClick _ s _) = toPress s



class (Pressable p) => Button b p | b -> p where
    button :: WindowSource (Event t) -> b -> Event t p

instance Button Key KeyPress where
    button w k = filterE matchKey $ key w
      where
        matchKey (KeyPress k' _ _ _) = k == k'

instance Button ScanCode KeyPress where
    button w sc = filterE matchKey $ key w
      where
        matchKey (KeyPress _ sc' _ _) = sc == sc'

instance Button MouseButton MouseClick where
    button w mb = filterE matchButton $ mouse w
      where
        matchButton (MouseClick mb' _ _) = mb == mb'

