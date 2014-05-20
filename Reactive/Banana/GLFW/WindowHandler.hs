-- | This module defines `WindowHandler`, a source of GLFW Events that are
-- received through a particular `GLFW.Window`. It is not necessary to
-- import this module unless you need to register callbacks outside of
-- reactive-banana.
--
-- This module should usually be imported qualified because it exports
-- names that conflict with the rest of the library.


module Reactive.Banana.GLFW.WindowHandler
(
    module Reactive.Banana.GLFW.Types,

    -- * WindowHandlers
    WindowHandler(..),
    windowHandler,
)
where

import Graphics.UI.GLFW as GLFW
import Reactive.Banana hiding ( Identity )
import Reactive.Banana.Frameworks

import Reactive.Banana.GLFW.Types
import Control.Event.Handler


data WindowHandler = WindowHandler
    { window      :: GLFW.Window
    , refresh     :: AddHandler ()
    , close       :: AddHandler ()
    , focus       :: AddHandler Bool
    , iconify     :: AddHandler Bool
    , move        :: AddHandler (Int, Int)
    , resize      :: AddHandler (Int, Int)
    , char        :: AddHandler Char
    , keyEvent    :: AddHandler KeyEvent
    , mouseEvent  :: AddHandler MouseEvent
    , cursorMove  :: AddHandler (Double, Double)
    , cursorEnter :: AddHandler Bool
    }


-- | Obtain a `WindowHandler` for a `GLFW.Window`.
--
-- This will register every GLFW callback for this window. When you create
-- a WindowHandler for a Window, you invalidate every WindowHandler that
-- was previously created for that Window.
--
windowHandler :: GLFW.Window -> IO WindowHandler
windowHandler w = WindowHandler w
    <$> hc1 (GLFW.setWindowRefreshCallback w)
    <*> hc1 (GLFW.setWindowCloseCallback w)
    <*> (fmap (== FocusState'Focused)
        <$> hc2 (GLFW.setWindowFocusCallback w))
    <*> (fmap (== IconifyState'Iconified)
        <$> hc2 (GLFW.setWindowIconifyCallback w))
    <*> hc3 (GLFW.setWindowPosCallback w)
    <*> hc3 (GLFW.setWindowSizeCallback w)
    <*> hc2 (GLFW.setCharCallback w)
    <*> handleCallback (\f _ k i s m -> f $ ButtonEvent (k, SC i) s (listModKeys m))
            (GLFW.setKeyCallback w)
    <*> handleCallback (\f _ mb s m -> f $ ButtonEvent mb s (listModKeys m))
            (GLFW.setMouseButtonCallback w)
    <*> hc3 (GLFW.setCursorPosCallback w)
    <*> (fmap (== CursorState'InWindow)
        <$> hc2 (GLFW.setCursorEnterCallback w))
  where
    hc1 = handleCallback $ \fire _ -> fire ()
    hc2 = handleCallback $ \fire _ a -> fire a
    hc3 = handleCallback $ \fire _ a b -> fire (a, b)



listModKeys :: ModifierKeys -> [ModKey]
listModKeys (ModifierKeys sh c a s) = map snd $ filter fst
    [ (sh, Shift)
    , (c,  Ctrl)
    , (a,  Alt)
    , (s,  Super)
    ]

-- | Create an `AddHandler'` for a callback in the shape provided by GLFW-b
handleCallback
    :: ((a -> IO ()) -> cb)
    -> (Maybe cb -> IO ())
    -> IO (AddHandler a)
handleCallback f cb = do
    (ah, fire) <- newAddHandler
    liftIO $ cb $ Just $ f fire
    return ah
