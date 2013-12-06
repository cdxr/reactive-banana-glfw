-- | This module provides Events and Behaviors for mouse movement.
--
-- Every position emitted by Events and Behaviors in this module are subject
-- to the `CursorOrigin` defined on the given `WindowE`.
--
module Reactive.Banana.GLFW.Mouse
(
    -- * Cursor
    --
    -- | The value of the /cursor/ is a @(Double, Double)@ that represents the
    -- last position of the mouse within the window.
    -- 
    cursorPos,
    cursorMove,
    I.cursorEnter,

    -- * Mouse
    -- | The value of the /mouse/ is a @Maybe (Double, Double)@ that is @Just@
    -- the cursor value or @Nothing@ if the mouse has exited the window.
    --
    mousePos,
    mouseMove,

    -- * Origin
    I.CursorOrigin(..),
    I.cursorOrigin,
    I.setCursorOrigin,
) where

import Reactive.Banana
import Reactive.Banana.GLFW.Internal.Utils
import Reactive.Banana.GLFW.Internal.WindowE as I


-- | @cursorPos w@ is the cursor position of @window w@.
cursorPos :: WindowE t -> Behavior t (Double, Double)
cursorPos = stepper (0,0) . cursorMove


-- | @cursorMove w@ emits the cursor position every time it moves within
-- @window w@.
cursorMove :: WindowE t -> Event t (Double, Double)
cursorMove w = case cursorOrigin w of
    TopLeft    -> _cursorMove w
    BottomLeft -> flipY <$> size w <@> _cursorMove w
  where
    flipY :: (Int, Int) -> (Double, Double) -> (Double, Double)
    flipY (_, height) (x, y) = (x, fromIntegral height - y)


-- | @mouseMove w@ emits @Just@ the mouse position whenever the mouse moves
-- inside @window w@, and emits @Nothing@ when the mouse exits the window.
--
-- This is a combination of `cursorMove` and `cursorEnter`.
mouseMove :: WindowE t -> Event t (Maybe (Double, Double))
mouseMove w = spigot (cursorEnter w) (cursorMove w)


-- | @mousePos w@ is @Just@ the position of the mouse in @window w@
-- or @Nothing@ when the mouse is not in the window.
mousePos :: WindowE t -> Behavior t (Maybe (Double, Double))
mousePos = stepper Nothing . mouseMove
