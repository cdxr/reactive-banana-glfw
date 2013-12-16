module Reactive.Banana.GLFW.AddHandler where

import Data.Monoid

import Reactive.Banana
import Reactive.Banana.Frameworks


-- | A concrete `AddHandler`. This is useful for its `Functor` and `Monoid`
-- instances.
newtype AddHandler' a = AddHandler' { registerCallback :: AddHandler a }

instance Functor AddHandler' where
    fmap f ah = AddHandler' $ \cb -> registerCallback ah (cb . f)

instance Monoid (AddHandler' a) where
    mempty = AddHandler' $ \_ -> return (return ())
    a `mappend` b = AddHandler' $ \cb -> do
        da <- registerCallback a cb
        db <- registerCallback b cb
        return $ db >> da
        

fromAddHandler' :: Frameworks t => AddHandler' a -> Moment t (Event t a)
fromAddHandler' = fromAddHandler . registerCallback

fromChanges' :: Frameworks t => a -> AddHandler' a -> Moment t (Behavior t a)
fromChanges' a = fromChanges a . registerCallback
