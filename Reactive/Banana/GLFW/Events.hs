module Reactive.Banana.GLFW.Events where

import Control.Monad ( void )

import Reactive.Banana
import Graphics.UI.GLFW

import Reactive.Banana.GLFW.EventSource


data ButtonState = Press | Release
    deriving (Show, Eq, Ord)

pressed :: Event t ButtonState -> Event t ()
pressed = void . filterE (== Press)

released :: Event t ButtonState -> Event t ()
released = void . filterE (== Release)


class Button a where
    button :: EventSource t -> a -> Event t ButtonState

instance Button Key where
    button es k = filterJust . fmap getState . filterE matchKey $ key es
      where
        matchKey (_, k', _, _, _) = k == k'
        getState (_, _, _, s, _) = case s of
            KeyState'Pressed   -> Just Press
            KeyState'Released  -> Just Release
            KeyState'Repeating -> Nothing


data Flag
    = MonitorConnect (Maybe Monitor)
    | WindowFocus (Maybe Window)
    | WindowIconify (Maybe Window)
    deriving (Show, Eq, Ord)


flag :: EventSource t -> Flag -> Event t Bool
flag es f = case f of
    MonitorConnect mon ->
        mk (monitor es) mon MonitorState'Connected
    WindowFocus win ->
        mk (windowFocus es) win FocusState'Focused
    WindowIconify win ->
        mk (windowIconify es) win IconifyState'Iconified
  where
    mk :: (Eq m, Eq b) => Event t (m, b) -> Maybe m -> b -> Event t Bool
    mk e mmatch true = (== true) . snd <$> filterE (match . fst) e
      where
          match = maybe (const True) (==) mmatch
