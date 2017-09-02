module Reactive.Banana.GLFW.Internal.Utils where

import Reactive.Banana


-- | @spigot b a@ emits @Nothing@ when @b@ emits @False@. It emits @Just x@
-- when @a@ emits @x@ iff the previous value emitted by @b@ was @True@.
spigot :: (MonadMoment m) => Event Bool -> Event a -> m (Event (Maybe a))
spigot b a = do
    gateOpen <- stepper False b
    return $ 
        unionWith
            (\a b -> a)
            (Just <$> whenE gateOpen a)
            (Nothing <$ filterE not b)
