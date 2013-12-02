{-# LANGUAGE Rank2Types #-}

import Control.Applicative
import Control.Monad

import System.Exit       ( exitSuccess )
import Control.Exception ( bracket )

import Graphics.UI.GLFW as GLFW

import Reactive.Banana as R
import Reactive.Banana.Frameworks
import Reactive.Banana.GLFW.EventSource



main :: IO ()
main = withWindow $ \w -> adapt w $ \es ->
    return $ exit es `union` (putStrLn <$> eventShow es)
  where
    exit es = exitSuccess <$ keyEscape es
    keyEscape es = filterE (\(_,k,_,_,_) -> k == Key'Escape) (key es)


withWindow :: (GLFW.Window -> IO ()) -> IO ()
withWindow = bracket create destroy
  where
    create = do
        True <- GLFW.init

        let title = "basic reactivebanana-glfw example"
        Just win <- GLFW.createWindow 200 200 title Nothing Nothing

        GLFW.makeContextCurrent $ Just win

        return win

    destroy w = do
        GLFW.destroyWindow w
        GLFW.terminate


eventShow :: EventSource t -> Event t String
eventShow es = show <$> windowFocus es


type NetworkDescription t = EventSource t -> Moment t (Event t (IO ()))


adapt :: GLFW.Window -> (forall t. NetworkDescription t) -> IO ()
adapt w netdesc = do
    network <- compile $ reactimate =<< netdesc =<< eventSource w

    actuate network
    forever GLFW.pollEvents
