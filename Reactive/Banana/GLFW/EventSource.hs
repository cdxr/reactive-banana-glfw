{-# LANGUAGE NoMonomorphismRestriction #-}
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

import Control.Monad


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


-- | Given a list of `GLFW.Window`s, create an `EventSource` that registers
-- callbacks for all those windows.
eventSource :: (Frameworks t) => [GLFW.Window] -> Moment t (EventSource t)
eventSource ws =
    EventSource
        <$> hc2 GLFW.setErrorCallback
        <*> hc2 GLFW.setMonitorCallback 
        <*> hc1 (forEachWindow GLFW.setWindowRefreshCallback)
        <*> hc1 (forEachWindow GLFW.setWindowCloseCallback)
        <*> hc2 (forEachWindow GLFW.setWindowFocusCallback)
        <*> hc2 (forEachWindow GLFW.setWindowIconifyCallback)
        <*> hc3 (forEachWindow GLFW.setWindowPosCallback)
        <*> hc3 (forEachWindow GLFW.setWindowSizeCallback)
        <*> hc5 (forEachWindow GLFW.setKeyCallback)
        <*> hc2 (forEachWindow GLFW.setCharCallback)
        <*> hc4 (forEachWindow GLFW.setMouseButtonCallback)
        <*> hc3 (forEachWindow GLFW.setCursorPosCallback)
        <*> hc2 (forEachWindow GLFW.setCursorEnterCallback)
  where
    forEachWindow f x = forM_ ws $ \w -> f w x

