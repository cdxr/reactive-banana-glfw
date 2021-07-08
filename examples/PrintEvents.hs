{-# LANGUAGE Rank2Types #-}

--import Control.Applicative
import Control.Monad

import System.Exit       ( exitSuccess )
import Control.Exception ( bracket )

import Graphics.UI.GLFW as GLFW

import Reactive.Banana as R
import Reactive.Banana.Frameworks

import Reactive.Banana.GLFW


main :: IO ()
main = withWindow $ \window -> do
    h <- windowHandler window
    network <- compile $ do
        closeE <- close h
        reactimate $ exitSuccess <$ closeE
        keyE <- keyEvent h
        reactimate $ (GLFW.setWindowShouldClose window True >> exitSuccess) <$ filterE (match Key'Escape) keyE
        reactimate $ print <$> keyE

        c <- cursor h TopLeft
        reactimate $ putStrLn . ("Cursor: " ++) . show <$> mouseMove c
        sc <- scroll h
        reactimate $ putStrLn . ("Scroll: " ++) . show <$> sc

    actuate network
    forever GLFW.pollEvents


withWindow :: (GLFW.Window -> IO ()) -> IO ()
withWindow = withGLFW . bracket create destroy
  where
    create = do
        let title = "reactive-banana-glfw example: PrintEvents.hs"
        Just win <- GLFW.createWindow 200 200 title Nothing Nothing

        GLFW.makeContextCurrent $ Just win

        return win

    destroy = GLFW.destroyWindow
