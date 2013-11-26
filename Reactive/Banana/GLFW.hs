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
    , windowSize    :: Event t (Int, Int)
    , key           :: Event t (Key, Int, KeyState, ModifierKeys)
    , char          :: Event t Char
    , mouseButton   :: Event t (MouseButton, MouseButtonState, ModifierKeys)
    , cursorPos     :: Event t (Double, Double)
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

    EventSource
        <$> fromAddHandler windowRefresh
        <*> fromAddHandler windowSize
        <*> fromAddHandler key
        <*> fromAddHandler char
        <*> fromAddHandler mouseButton
        <*> fromAddHandler cursorPos
