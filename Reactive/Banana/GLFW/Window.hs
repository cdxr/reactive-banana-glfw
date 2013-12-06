{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Reactive.Banana.GLFW.Window
(
    module Reactive.Banana.GLFW.Types,

    -- * WindowE
    WindowE,
    windowEvents,
    fromWindowHandler,
    window,

    -- * Window Events
    -- ** Properties
    refresh,
    close,
    focus,
    iconify,

    -- ** Geometry
    move,
    resize,
    size,

    -- * Input
    -- ** Buttons
    char,
    keyChange,
    mouseChange,
    -- ** Cursor
    cursorMove,
    cursorMove',
    cursorPos,
    cursorEnter,
    -- *** Origin
    CursorOrigin(..),
    cursorOrigin,
    setCursorOrigin,
)
where

import Reactive.Banana
import Reactive.Banana.GLFW.Types
import Reactive.Banana.GLFW.Internal.WindowE


-- | @cursorMove' w@ emits @Just@ the cursor position whenever the cursor moves
-- inside @window w@, and emits @Nothing@ when the cursor exits the
-- window.
--
-- This is a combination of `cursorMove` and `cursorEnter`.
cursorMove' :: WindowE t -> Event t (Maybe (Double, Double))
cursorMove' w = (Just <$> whenE cursorInWindow (cursorMove w))
        `union` (Nothing <$ filterE not (cursorEnter w))
  where
    cursorInWindow = stepper False $ cursorEnter w


-- | @cursorPos w@ is @Just@ the position of the cursor in @window w@
-- or @Nothing@ when the cursor is not in the window.
cursorPos :: WindowE t -> Behavior t (Maybe (Double, Double))
cursorPos = stepper Nothing . cursorMove'
