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
    Cursor,
    cursor,   

    cursorPos,
    cursorMove,
    cursorEnter,

    -- * Mouse
    -- | The value of the /mouse/ is a @Maybe (Double, Double)@ that is @Just@
    -- the cursor value or @Nothing@ if the mouse has exited the window.
    --
    mousePos,
    mouseMove,

    -- * Origin
    CursorOrigin(..),
) where


import qualified Graphics.UI.GLFW as GLFW

import Reactive.Banana
import Reactive.Banana.Frameworks

import Reactive.Banana.GLFW.Window ( size )

import Reactive.Banana.GLFW.AddHandler
import qualified Reactive.Banana.GLFW.WindowHandler as WH


-- | The origin of the screen coordinates, used for reporting cursor movements.
-- The default is @TopLeft@.
--
data CursorOrigin
    = TopLeft       -- ^ (0,0) is at the top-left corner (typical GLFW)
    | BottomLeft    -- ^ (0,0) is at the bottom-left corner
    deriving (Show, Read, Eq, Ord)

data Cursor t = Cursor
    -- | @cursorMove c@ emits the position of the cursor @c@ whenever it
    -- changes.
    { cursorMove  :: Event t (Double, Double)

    -- | @cursorPos c@ is the position of the cursor @c@. When the mouse
    -- leaves the window, the cursor position is equal to the most recent
    -- position of the mouse in the window.
    , cursorPos   :: Behavior t (Double, Double)

    -- | @cursorEnter c@ emits an event when the cursor enters the window
    -- (@True@) and when it exits the window (@False@).
    , cursorEnter :: Event t Bool
    }

cursor :: (Frameworks t) => WH.WindowHandler -> CursorOrigin -> Moment t (Cursor t)
cursor w co = do
    initWindowSize <- liftIO $ GLFW.getWindowSize (WH.window w)
    initPos <- liftIO $ adjust initWindowSize <$> GLFW.getCursorPos (WH.window w)
    enter <- fromAddHandler' $ WH.cursorEnter w

    bSize <- size w
    rawCursorMove <- fromAddHandler' (WH.cursorMove w)
    let move = adjust <$> bSize <@> rawCursorMove

    return Cursor { cursorMove = move
                  , cursorPos  = stepper initPos move
                  , cursorEnter = enter
                  }
  where
    adjust :: (Int, Int) -> (Double, Double) -> (Double, Double)
    adjust = case co of
        TopLeft    -> const id
        BottomLeft -> \(_width, height) (x, y) -> (x, fromIntegral height - y)


-- | @mouseMove c@ emits @Just p@ whenever @cursorMove c@ emits @p@, and emits
-- @Nothing@ whenever the cursor leaves the window.
--
-- This is a combination of `cursorMove` and `cursorEnter`.
mouseMove :: Cursor t -> Event t (Maybe (Double, Double))
mouseMove c = (Nothing <$ filterE not (cursorEnter c))
      `union` (Just <$> cursorMove c)
--mouseMove w = spigot (cursorEnter w) (cursorMove w)


-- | @mousePos c@ is @Just@ the value of @cursorPos c@ or @Nothing@ when the
-- mouse has left the window.
mousePos :: Cursor t -> Behavior t (Maybe (Double, Double))
mousePos = stepper Nothing . mouseMove
