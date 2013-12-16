module Reactive.Banana.GLFW.Internal.Utils where

import Reactive.Banana


-- | @spigot b a@ emits @Nothing@ when @b@ emits @False@. It emits @Just x@
-- when @a@ emits @x@ iff the previous value emitted by @b@ was @True@.
spigot :: Event t Bool -> Event t a -> Event t (Maybe a)
spigot b a = (Just <$> whenE gateOpen a)
     `union` (Nothing <$ filterE not b)
  where
    gateOpen = stepper False b
