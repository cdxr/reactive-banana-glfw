module Reactive.Banana.GLFW.Utils where

import Control.Monad
import Control.Exception

import Graphics.UI.GLFW as GLFW


withGLFW :: IO () -> IO ()
withGLFW = bracket_ (assertIO =<< GLFW.init) GLFW.terminate
  where
    assertIO b = unless b $ ioError $ userError "withGLFW"
