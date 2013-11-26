{-# LANGUAGE Rank2Types #-}

module Reactive.Banana.GLFW
(
    EventSource(..),
    windowEventSource,
)
where

import Graphics.UI.GLFW as GLFW
import Reactive.Banana as R
import Reactive.Banana.Frameworks as R


-- TODO  add support for error and monitor callbacks
--       ^ are these even useful?


data EventSource t = EventSource
    { windowRefresh :: Event t ()
    , windowClose   :: Event t ()
    , windowFocus   :: Event t FocusState
    , windowIconify :: Event t IconifyState
    , windowPos     :: Event t (Int, Int)
    , windowSize    :: Event t (Int, Int)
    , key           :: Event t (Key, Int, KeyState, ModifierKeys)
    , char          :: Event t Char
    , mouseButton   :: Event t (MouseButton, MouseButtonState, ModifierKeys)
    , cursorPos     :: Event t (Double, Double)
    , cursorEnter   :: Event t CursorState
    }


handleCallback
    :: (Frameworks t)
    => (Maybe cb -> IO ())
    -> ((a -> IO ()) -> cb)
    -> Moment t (Event t a)
handleCallback cb f = do
    (ah, fire) <- newEvent
    liftIO $ cb $ Just $ f fire
    return ah


windowEventSource :: forall t. Frameworks t => GLFW.Window -> Moment t (EventSource t)
windowEventSource w = do
    windowRefresh <- handleCallback (GLFW.setWindowRefreshCallback w) $
            \fire _ -> fire ()

    windowClose <- handleCallback (GLFW.setWindowCloseCallback w) $
            \fire _ -> fire ()

    windowFocus <- handleCallback (GLFW.setWindowFocusCallback w) $
            \fire _ st -> fire st

    windowIconify <- handleCallback (GLFW.setWindowIconifyCallback w) $
            \fire _ st -> fire st

    windowPos <- handleCallback (GLFW.setWindowPosCallback w) $
            \fire _ x y -> fire (x,y)

    windowSize <- handleCallback (GLFW.setWindowSizeCallback w) $
            \fire _ x y -> fire (x,y)

    key <- handleCallback (GLFW.setKeyCallback w) $
            \fire _ key i st mods -> fire (key, i, st, mods)

    char <- handleCallback (GLFW.setCharCallback w) $
            \fire _ char -> fire char

    mouseButton <- handleCallback (GLFW.setMouseButtonCallback w) $
            \fire _ mb st mods -> fire (mb, st, mods)

    cursorPos <- handleCallback (GLFW.setCursorPosCallback w) $
            \fire _ x y -> fire (x, y)

    cursorEnter <- handleCallback (GLFW.setCursorEnterCallback w) $
            \fire _ st -> fire st


    return $ EventSource
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
