module Reactive.Banana.GLFW.WindowHandler
(
    module Reactive.Banana.GLFW.Types,
    module Reactive.Banana.GLFW.AddHandler,

    -- * WindowHandlers
    WindowHandler(..),
    windowHandler,
)
where

import Graphics.UI.GLFW as GLFW
import Reactive.Banana hiding ( Identity )
import Reactive.Banana.Frameworks

import Reactive.Banana.GLFW.Types
import Reactive.Banana.GLFW.AddHandler


data WindowHandler = WindowHandler
    { window      :: GLFW.Window
    , refresh     :: AddHandler' ()
    , close       :: AddHandler' ()
    , focus       :: AddHandler' Bool
    , iconify     :: AddHandler' Bool
    , move        :: AddHandler' (Int, Int)
    , resize      :: AddHandler' (Int, Int)
    , char        :: AddHandler' Char
    , keyEvent    :: AddHandler' KeyEvent
    , mouseEvent  :: AddHandler' MouseEvent
    , cursorMove  :: AddHandler' (Double, Double)
    , cursorEnter :: AddHandler' Bool
    }


-- | Obtain a `WindowHandler` for a `GLFW.Window`.
--
-- You only need a `WindowHandler` if you need to register callbacks outside
-- of reactive-banana.
--
-- Use `registerCallback` on the components of the `WindowHandler` for
-- traditional `IO` callbacks. Then use `eventsFromHandlers` to obtain a
-- `WindowEvents` to use reactive-banana as well.
--
-- Note that this will register every callback associated with this Window.
-- If a callback is registered directly with GLFW instead of this
-- `WindowHandler`, it will stop working for this `WindowHandler`.
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
    , (c,  Ctl)
    , (a,  Alt)
    , (s,  Super)
    ]

-- | Create an `AddHandler'` for a callback in the shape provided by GLFW-b
handleCallback
    :: ((a -> IO ()) -> cb)
    -> (Maybe cb -> IO ())
    -> IO (AddHandler' a)
handleCallback f cb = do
    (ah, fire) <- newAddHandler
    liftIO $ cb $ Just $ f fire
    return $ AddHandler' ah
