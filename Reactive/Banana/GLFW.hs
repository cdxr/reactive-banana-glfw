{-# LANGUAGE Rank2Types #-}

module Reactive.Banana.GLFW
(
    EventSource(..),
    windowEventSource,
)
where

import Control.Applicative
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
    :: (Maybe cb -> IO ())
    -> ((a -> IO ()) -> cb)
    -> IO (AddHandler a)
handleCallback cb f = do
    (ah, fire) <- newAddHandler
    cb $ Just $ f fire
    return ah


windowEventSource :: forall t. Frameworks t => GLFW.Window -> Moment t (EventSource t)
windowEventSource w = do
    windowRefresh <- liftIO $
        handleCallback (GLFW.setWindowRefreshCallback w) $
            \fire _ -> fire ()

    windowClose <- liftIO $
        handleCallback (GLFW.setWindowCloseCallback w) $
            \fire _ -> fire ()

    windowFocus <- liftIO $
        handleCallback (GLFW.setWindowFocusCallback w) $
            \fire _ st -> fire st

    windowIconify <- liftIO $
        handleCallback (GLFW.setWindowIconifyCallback w) $
            \fire _ st -> fire st

    windowPos <- liftIO $
        handleCallback (GLFW.setWindowPosCallback w) $
            \fire _ x y -> fire (x,y)

    windowSize <- liftIO $
        handleCallback (GLFW.setWindowSizeCallback w) $
            \fire _ x y -> fire (x,y)

    key <- liftIO $
        handleCallback (GLFW.setKeyCallback w) $
            \fire _ key i st mods -> fire (key, i, st, mods)

    char <- liftIO $ 
        handleCallback (GLFW.setCharCallback w) $
            \fire _ char -> fire char

    mouseButton <- liftIO $
        handleCallback (GLFW.setMouseButtonCallback w) $
            \fire _ mb st mods -> fire (mb, st, mods)

    cursorPos <- liftIO $
        handleCallback (GLFW.setCursorPosCallback w) $
            \fire _ x y -> fire (x, y)

    cursorEnter <- liftIO $
        handleCallback (GLFW.setCursorEnterCallback w) $
            \fire _ st -> fire st


    EventSource
        <$> fromAddHandler windowRefresh
        <*> fromAddHandler windowClose
        <*> fromAddHandler windowFocus
        <*> fromAddHandler windowIconify
        <*> fromAddHandler windowPos
        <*> fromAddHandler windowSize
        <*> fromAddHandler key
        <*> fromAddHandler char
        <*> fromAddHandler mouseButton
        <*> fromAddHandler cursorPos
        <*> fromAddHandler cursorEnter
