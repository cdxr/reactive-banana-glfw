module Reactive.Banana.GLFW.Internal.Utils where

import Reactive.Banana


-- | @spigot b a@ emits @Nothing@ when @b@ emits @False@. It emits @Just x@
-- when @a@ emits @x@ if the previous value emitted by @b@ was @True@.
spigot :: MonadMoment m => Event Bool -> Event a -> m (Event (Maybe a))
spigot b a = f <$> (stepper False b >>= valueB)
    where f gateOpen = if gateOpen
                       then Just <$> a
                       else Nothing <$ a
