{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Reactive.Banana.GLFW.Events
(
    -- * Button
    Button(..),
    -- ** Press
    Press(..),
    PressEvent(..),
    press,
    release,
    hold,
    -- ** ModKey
    ModEvent(..),
    mods,

    -- * Cursor
    cursorMove'
)
where

import Data.Maybe ( isNothing )

import Reactive.Banana
import Graphics.UI.GLFW

import Reactive.Banana.GLFW.Window


data Press = Release | Press
    deriving (Show, Eq, Ord, Bounded, Enum)


-- | Class of types that represent the pressing or releasing of a `Button`.
class PressEvent b where
    getPress :: b -> Maybe Press

press :: (PressEvent a) => a -> Bool
press = (== Just Press) . getPress

release :: (PressEvent a) => a -> Bool
release = (== Just Release) . getPress

hold :: KeyPress -> Bool
hold = isNothing . getPress


instance PressEvent KeyPress where
    getPress (KeyPress _ _ s _) = case s of
        KeyState'Pressed   -> Just Press
        KeyState'Released  -> Just Release
        KeyState'Repeating -> Nothing

instance PressEvent MouseClick where
    getPress (MouseClick _ s _) = Just $ case s of
        MouseButtonState'Pressed  -> Press
        MouseButtonState'Released -> Release



class ModEvent a where
    modkey :: ModKey -> a -> Bool

instance ModEvent KeyPress where
    modkey m (KeyPress _ _ _ ms) = m `elem` ms

instance ModEvent MouseClick where
    modkey m (MouseClick _ _ ms) = m `elem` ms

-- | @mods ms a@ is True iff for all @m@ in @ms@, @modkey m a@.
--
-- @mods [] a@ is True only when @a@ includes no ModKeys.
mods :: (ModEvent a) => [ModKey] -> a -> Bool
mods ms a = all (\m -> modkey m a == elem m ms) [Shift .. Super]

    
class (PressEvent e, ModEvent e) => Button b e | b -> e where
    button :: WindowEvents t -> b -> Event t e

instance Button Key KeyPress where
    button w k = filterE matchKey $ keyChange w
      where
        matchKey (KeyPress k' _ _ _) = k == k'

instance Button ScanCode KeyPress where
    button w sc = filterE matchKey $ keyChange w
      where
        matchKey (KeyPress _ sc' _ _) = sc == sc'

instance Button MouseButton MouseClick where
    button w mb = filterE matchButton $ mouseChange w
      where
        matchButton (MouseClick mb' _ _) = mb == mb'



-- | @cursor w@ is @Just@ the cursor position, or @Nothing@ if the cursor
-- is not on the screen.
cursorMove' :: WindowEvents t -> Event t (Maybe (Double, Double))
cursorMove' w = (Just <$> whenE cursorInWindow (cursorMove w))
        `union` (Nothing <$ filterE not (cursorEnter w))
  where
    cursorInWindow = stepper False $ cursorEnter w
