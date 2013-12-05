{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Reactive.Banana.GLFW.Window
(
    module Reactive.Banana.GLFW.Types,

    -- * WindowEvents
    WindowEvents(..),
    windowEvents,
    fromWindowHandler,
)
where

import Control.Monad

import Graphics.UI.GLFW as GLFW

import Reactive.Banana
import Reactive.Banana.Frameworks

import Reactive.Banana.GLFW.Types
import Reactive.Banana.GLFW.AddHandler
import qualified Reactive.Banana.GLFW.WindowHandler as WH


data WindowEvents t = WindowEvents
    { window      :: GLFW.Window
    , refresh     :: Event t ()
    , close       :: Event t ()
    , focus       :: Event t Bool
    , iconify     :: Event t Bool
    , move        :: Event t (Int, Int)
    , resize      :: Event t (Int, Int)
    , char        :: Event t Char
    , keyChange   :: Event t KeyPress
    , mouseChange :: Event t MouseClick
    , cursorMove  :: Event t (Double, Double)
    , cursorEnter :: Event t Bool
    , size        :: Behavior t (Int, Int)
    }


-- | Obtain a `WindowEvents` for a `GLFW.Window`.
--
-- This will register every callback associated with this Window.
-- If you need to register additional callbacks outside of reactive-banana,
-- see `WH.windowHandler`.
--
windowEvents :: (Frameworks t) => GLFW.Window -> Moment t (WindowEvents t)
windowEvents = fromWindowHandler <=< liftIO . WH.windowHandler


-- | Obtain a `WindowEvents` from a `WH.WindowHandler`.
fromWindowHandler :: (Frameworks t) => WH.WindowHandler -> Moment t (WindowEvents t)
fromWindowHandler wh = do
    let w = WH.window wh

    eResize <- fromAddHandler' (WH.resize wh)
    size0 <- liftIO $ GLFW.getWindowSize w
    let bSize = stepper size0 eResize

    WindowEvents w
        <$> fromAddHandler' (WH.refresh wh)
        <*> fromAddHandler' (WH.close wh)
        <*> fromAddHandler' (WH.focus wh)
        <*> fromAddHandler' (WH.iconify wh)
        <*> fromAddHandler' (WH.move wh)
        <*> pure eResize
        <*> fromAddHandler' (WH.char wh)
        <*> fromAddHandler' (WH.keyChange wh)
        <*> fromAddHandler' (WH.mouseChange wh)
        <*> fromAddHandler' (WH.cursorMove wh)
        <*> fromAddHandler' (WH.cursorEnter wh)
        <*> pure bSize
