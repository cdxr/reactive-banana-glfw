-- | This module provides `Cursor t`, a source of Events and Behaviors related
-- to mouse input.
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

import Control.Event.Handler
import qualified Reactive.Banana.GLFW.WindowHandler as WH


-- | The origin of the screen coordinates, used for reporting cursor movements.
-- The default is @TopLeft@.
--
data CursorOrigin
    = TopLeft       -- ^ (0,0) is at the top-left corner (typical GLFW)
    | BottomLeft    -- ^ (0,0) is at the bottom-left corner
    deriving (Show, Read, Eq, Ord)

data Cursor = Cursor {
    -- | @cursorMove c@ emits the position of the cursor @c@ whenever it
    -- changes.
      cursorMove  :: Event (Double, Double)

    -- | @cursorPos c@ is the position of the cursor @c@. When the mouse
    -- leaves the window, the cursor position is equal to the most recent
    -- position of the mouse in the window.
    , cursorPos   :: Behavior (Double, Double)

    -- | @cursorEnter c@ emits an event when the cursor enters the window
    -- (@True@) and when it exits the window (@False@).
    , cursorEnter :: Event Bool
    }

cursor :: WH.WindowHandler -> CursorOrigin -> MomentIO Cursor
cursor w co = do
    initWindowSize <- liftIO $ GLFW.getWindowSize (WH.window w)
    initPos <- liftIO $ adjust initWindowSize <$> GLFW.getCursorPos (WH.window w)
    enter <- fromAddHandler $ WH.cursorEnter w

    bSize <- size w
    rawCursorMove <- fromAddHandler (WH.cursorMove w)
    let move = adjust <$> bSize <@> rawCursorMove

    cursPos <- stepper initPos move

    return Cursor { cursorMove = move
                  , cursorPos  = cursPos
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
mouseMove :: Cursor -> Event (Maybe (Double, Double))
mouseMove c = 
    unionWith
        (\e m -> m)
        (Nothing <$ filterE not (cursorEnter c))
        (Just <$> cursorMove c)
--mouseMove w = spigot (cursorEnter w) (cursorMove w)


-- | @mousePos c@ is @Just@ the value of @cursorPos c@ or @Nothing@ when the
-- mouse has left the window.
mousePos :: (MonadMoment m) => Cursor -> m (Behavior (Maybe (Double, Double)))
mousePos c = stepper Nothing (mouseMove c)
