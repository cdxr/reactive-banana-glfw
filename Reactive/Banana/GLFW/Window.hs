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

    -- * Events
    refresh,
    close,
    focus,
    iconify,

    -- * Position
    --position,
    move,
    move',

    -- * Size
    size,
    resize,

    -- * Buttons
    char,
    keyChange,
    mouseChange,
) where

import Reactive.Banana
import Reactive.Banana.GLFW.Types
import Reactive.Banana.GLFW.Internal.Utils
import Reactive.Banana.GLFW.Internal.WindowE


-- | @move' w@ emits @Just@ the window position whenever @window w@ moves,
-- and emits @Nothing@ when the window is iconified.
move' :: WindowE t -> Event t (Maybe (Int, Int))
move' w = spigot (not <$> iconify w) (move w)


{- TODO
-- | @position w@ is @Just@ the position of @window w@ or @Nothing@ if the
-- window is iconified.
position :: WindowE t -> Behavior t (Maybe (Double, Double))
position = undefined
-}

