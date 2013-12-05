{-# LANGUAGE Rank2Types #-}

import Control.Applicative
import Control.Monad

import System.Exit       ( exitSuccess )
import Control.Exception ( bracket )

import Graphics.UI.GLFW as GLFW

import Reactive.Banana as R
import Reactive.Banana.Frameworks

import Reactive.Banana.GLFW


main :: IO ()
main = withWindow $ \window -> do
    network <- compile $ do
        w <- windowEvents window
        reactimate $ exitSuccess <$ filterE press (button w Key'Escape)
        reactimate $ putStrLn <$> showEvents w
    actuate network
    forever GLFW.pollEvents


showEvents :: WindowEvents t -> Event t String
showEvents w = unions
    [ "window refreshed"       <$  refresh w
    , "window closed"          <$  close w
    , label "window focused"   <$> focus w
    , label "window iconified" <$> iconify w
    , label "window pos"       <$> position w
    , label "window size"      <$> size w
    , show <$> key w
    , show <$> char w
    , show <$> mouse w
    , show <$> cursorPos w
    , show <$> cursorEnter w
    ]
  where
    label s a = s ++ ": " ++ show a


withWindow :: (GLFW.Window -> IO ()) -> IO ()
withWindow = withGLFW . bracket create destroy
  where
    create = do
        let title = "reactive-banana-glfw example: PrintEvents.hs"
        Just win <- GLFW.createWindow 200 200 title Nothing Nothing

        GLFW.makeContextCurrent $ Just win

        return win

    destroy = GLFW.destroyWindow
