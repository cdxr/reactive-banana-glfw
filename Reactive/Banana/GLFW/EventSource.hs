{-# LANGUAGE Rank2Types #-}

-- | This module defines the type `EventSource`, a collection that provides
-- an `Event` for each callbacks provided by GLFW-b. Each event emits
-- a tuple containing all of the arguments supplied to the callback.
-- Many users should prefer the higher-level API exported by
-- Reactive.Banana.GLFW.
module Reactive.Banana.GLFW.EventSource
(
    EventSource(..),
    eventSource,
)
where

import Graphics.UI.GLFW as GLFW
import Reactive.Banana as R
import Reactive.Banana.Frameworks as R


data EventSource t = EventSource
    { glfwError     :: Event t (Error, String)
    , monitor       :: Event t (Monitor, MonitorState)
    , windowRefresh :: Event t Window
    , windowClose   :: Event t Window
    , windowFocus   :: Event t (Window, FocusState)
    , windowIconify :: Event t (Window, IconifyState)
    , windowPos     :: Event t (Window, Int, Int)
    , windowSize    :: Event t (Window, Int, Int)
    , key           :: Event t (Window, Key, Int, KeyState, ModifierKeys)
    , char          :: Event t (Window, Char)
    , mouseButton   :: Event t (Window, MouseButton, MouseButtonState, ModifierKeys)
    , cursorPos     :: Event t (Window, Double, Double)
    , cursorEnter   :: Event t (Window, CursorState)
    }


handleCallback
    :: (Frameworks t)
    => ((a -> IO ()) -> cb)
    -> (Maybe cb -> IO ())
    -> Moment t (Event t a)
handleCallback f cb = do
    (ah, fire) <- newEvent
    liftIO $ cb $ Just $ f fire
    return ah


hc1 = handleCallback $ \fire a -> fire a
hc2 = handleCallback $ \fire a b -> fire (a, b)
hc3 = handleCallback $ \fire a b c -> fire (a, b, c)
hc4 = handleCallback $ \fire a b c d -> fire (a, b, c, d)
hc5 = handleCallback $ \fire a b c d e -> fire (a, b, c, d, e)


-- | An `EventSource` for a single `Window`.
eventSource :: forall t. Frameworks t => GLFW.Window -> Moment t (EventSource t)
eventSource w = do
    glfwError     <- hc2 GLFW.setErrorCallback
    monitor       <- hc2 GLFW.setMonitorCallback
    windowRefresh <- hc1 $ GLFW.setWindowRefreshCallback w
    windowClose   <- hc1 $ GLFW.setWindowCloseCallback w
    windowFocus   <- hc2 $ GLFW.setWindowFocusCallback w
    windowIconify <- hc2 $ GLFW.setWindowIconifyCallback w
    windowPos     <- hc3 $ GLFW.setWindowPosCallback w
    windowSize    <- hc3 $ GLFW.setWindowSizeCallback w
    key           <- hc5 $ GLFW.setKeyCallback w
    char          <- hc2 $ GLFW.setCharCallback w
    mouseButton   <- hc4 $ GLFW.setMouseButtonCallback w
    cursorPos     <- hc3 $ GLFW.setCursorPosCallback w
    cursorEnter   <- hc2 $ GLFW.setCursorEnterCallback w

    return $ EventSource
        glfwError
        monitor
        windowRefresh
        windowClose
        windowFocus
        windowIconify
        windowPos
        windowSize
        key
        char
        mouseButton
        cursorPos
        cursorEnter
