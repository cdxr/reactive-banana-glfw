--
-- | This is an /internal module/ that defines the `WindowE` type.
--
-- Users import this module at their own risk.
--
module Reactive.Banana.GLFW.Internal.WindowE where

import Control.Monad

import Graphics.UI.GLFW as GLFW

import Reactive.Banana
import Reactive.Banana.Frameworks

import Reactive.Banana.GLFW.Types
import Reactive.Banana.GLFW.AddHandler
import qualified Reactive.Banana.GLFW.WindowHandler as WH


-- | A type representing all event input from a `GLFW.Window`. A `WindowE`
-- is necessary to receive events and behaviors such as mouse input and
-- window size.
data WindowE t = WindowE {
    -- | @window w@ is the GLFW `GLFW.Window` referenced by @w@. All input
    -- into the window is made available to reactive-banana through @w@.
      window       :: GLFW.Window

    , cursorOrigin :: CursorOrigin

    -- | @refresh w@ is triggered whenever @window w@ needs to be redrawn,
    -- for example if the window has been exposed after having been covered by
    -- another window.
    , refresh      :: Event t ()

    -- | @close w@ is triggered when the user attempts to close @window w@,
    -- for example by clicking the close widget in the title bar.
    --
    -- When this is triggered, the close flag has been set and the `GLFW.Window`
    -- will close unless the flag is unset.
    , close        :: Event t ()

    -- | @focus w@ is triggered when @window w@ gains focus (@True@) or loses
    -- focus (@False@).
    --
    -- When the `GLFW.Window` loses focus, @keyChange w@ and @mouseChange w@
    -- will automatically emit release events for any buttons that were held
    -- down.
    , focus        :: Event t Bool

    -- | @iconify w@ is triggered when @window w@ is iconified (@True@) or
    -- restored (@False@).
    , iconify      :: Event t Bool

    -- | @move w@ emits the position of @window w@ whenever it is moved.
    , move         :: Event t (Int, Int)

    -- | @resize w@ emits the size of @window w@ whenever it is resized
    , resize       :: Event t (Int, Int)

    -- | @char w@ emits each Unicode character that is entered into @window w@.
    --
    -- This should only be used for text input. If you want events from a
    -- specific key, use `keyChange`.
    , char         :: Event t Char

    -- | @keyChange w@ emits a `KeyEvent` for every state change of every
    -- key while @window w@ has focus.
    --
    -- This should only be used for reacting to physical keys. For text
    -- input, use `char`.
    , keyEvent     :: Event t KeyEvent

    -- | @mouseChange w@ emits a `MouseEvent` for every state change of every
    -- mouse button while @window w@ has focus.
    , mouseEvent   :: Event t MouseEvent

    -- | @_cursorPos w@ is the continuous cursor position reported by GLFW.
    -- It is not exported so that users will use @cursorPos w@, which uses
    -- a different origin depending on @cursorOrigin w@.
    --
    -- If @cursorOrigin w = TopLeft@ then @cursorPos w = _cursorPos w@
    , _cursorPos  :: Behavior t (Double, Double)

    -- | @_cursorMove w@ is the raw cursor position reported by GLFW. It is
    -- not exported so that users will use @cursorMove w@, which uses
    -- a different origin depending on @cursorOrigin w@.
    --
    -- If @cursorOrigin w = TopLeft@ then @cursorMove w = _cursorMove w@
    , _cursorMove  :: Event t (Double, Double)

    -- | @cursorEnter w@ is triggered when the cursor enters the window
    -- (@True@) and when it exits the window (@False@).
    , cursorEnter  :: Event t Bool

    -- | @size w@ is the size of @window w@.
    , size         :: Behavior t (Int, Int)
    }


-- | Obtain a `WindowE` for a `GLFW.Window`.
--
-- This will register every callback associated with this Window.
-- If you need to register additional callbacks outside of reactive-banana,
-- see `WH.windowHandler`.
--
windowEvents :: (Frameworks t) => GLFW.Window -> Moment t (WindowE t)
windowEvents = fromWindowHandler <=< liftIO . WH.windowHandler


-- | Obtain a `WindowE` from a `WH.WindowHandler`.
fromWindowHandler :: (Frameworks t) => WH.WindowHandler -> Moment t (WindowE t)
fromWindowHandler wh = do
    let w = WH.window wh

    cursorPos  <- fromChanges (0,0) $ registerCallback $ WH.cursorMove wh
    cursorMove <- changes cursorPos

    --eResize <- 
    size0 <- liftIO $ GLFW.getWindowSize w
    let bSize = fromChanges size0 $ registerCallback $ WH.resize wh

    --let bSize = stepper size0 eResize

    WindowE w TopLeft
        <$> fromAddHandler' (WH.refresh wh)
        <*> fromAddHandler' (WH.close wh)
        <*> fromAddHandler' (WH.focus wh)
        <*> fromAddHandler' (WH.iconify wh)
        <*> fromAddHandler' (WH.move wh)
        <*> fromAddHandler' (WH.resize wh)
        <*> fromAddHandler' (WH.char wh)
        <*> fromAddHandler' (WH.keyEvent wh)
        <*> fromAddHandler' (WH.mouseEvent wh)
        <*> pure cursorPos
        <*> pure cursorMove
        <*> fromAddHandler' (WH.cursorEnter wh)
        <*> bSize


-- | The origin of the screen coordinates, used for reporting cursor movements.
-- The default is @TopLeft@.
--
data CursorOrigin
    = TopLeft       -- ^ (0,0) is at the top-left corner (typical GLFW)
    | BottomLeft    -- ^ (0,0) is at the bottom-left corner
    deriving (Show, Read, Eq, Ord)


setCursorOrigin :: CursorOrigin -> WindowE t -> WindowE t
setCursorOrigin co w = w { cursorOrigin = co }
