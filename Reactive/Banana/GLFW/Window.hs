{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Reactive.Banana.GLFW.Window
(
    module Reactive.Banana.GLFW.Types,

    -- * Events
    refresh,
    close,
    focus,
    iconify,

    -- * Position
    position,
    move,

    -- * Size
    size,
    resize,
) where


import qualified Graphics.UI.GLFW as GLFW

import Reactive.Banana
import Reactive.Banana.Frameworks

import Reactive.Banana.GLFW.Types
import Reactive.Banana.GLFW.Internal.Utils

import Reactive.Banana.GLFW.WindowHandler ( WindowHandler )
import qualified Reactive.Banana.GLFW.WindowHandler as WH


-- | @refresh w@ creates an event that is triggered whenever @window w@ needs
-- to be redrawn, for example if the window has been exposed after having been
-- covered by another window.
refresh :: (Frameworks t) => WindowHandler -> Moment t (Event t ())
refresh = fromAddHandler . WH.refresh


-- | @close w@ creates an event that is triggered when the user attempts to
-- close @window w@, for example by clicking the close widget in the title bar.
--
-- When this is triggered, the close flag has been set and the `GLFW.Window`
-- will close unless the flag is unset.
close :: (Frameworks t) => WindowHandler -> Moment t (Event t ())
close = fromAddHandler . WH.close


-- | @focus w@ creates an event that is triggered when @window w@ gains focus
-- (@True@) or loses focus (@False@).
--
-- When the `GLFW.Window` loses focus, @keyChange w@ and @mouseChange w@
-- will automatically emit release events for any buttons that were held
-- down.
focus :: (Frameworks t) => WindowHandler -> Moment t (Event t Bool)
focus = fromAddHandler . WH.focus


-- | @iconify w@ creates an event that is triggered when @window w@ is iconified
-- (@True@) or restored (@False@).
iconify :: (Frameworks t) => WindowHandler -> Moment t (Event t Bool)
iconify = fromAddHandler . WH.iconify


-- | @move w@ creates an event that emits the position of @window w@ whenever
-- it is moved.
move :: (Frameworks t) => WindowHandler -> Moment t (Event t (Int, Int))
move = fromAddHandler . WH.move


-- | @position w@ creates a behavior that is the position of @window w@.
position :: (Frameworks t) => WindowHandler -> Moment t (Behavior t (Int, Int))
position w = stepper <$> liftIO (GLFW.getWindowPos (WH.window w)) <*> move w


-- | @resize w@ creates an event that emits the size of @window w@ whenever it
-- is resized.
resize :: (Frameworks t) => WindowHandler -> Moment t (Event t (Int, Int))
resize = fromAddHandler . WH.resize


-- | @size w@ creates a behavior that is the size of @window w@.
size :: (Frameworks t) => WindowHandler -> Moment t (Behavior t (Int, Int))
size w = stepper <$> liftIO (GLFW.getWindowSize (WH.window w)) <*> resize w


-- | @move' w@ creates an event that emits @Just@ the window position whenever
-- @window w@ moves, and emits @Nothing@ when the window is iconified.
move' :: (Frameworks t) => WindowHandler -> Moment t (Event t (Maybe (Int, Int)))
move' w = spigot <$> (fmap not <$> iconify w) <*> move w


{- TODO

-- | @position w@ is @Just@ the window position or @Nothing@ if the window
-- is iconified.
position :: WindowHandler -> Behavior t (Maybe (Double, Double))
position = undefined
-}
