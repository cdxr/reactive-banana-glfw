{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Reactive.Banana.GLFW.Window
(
    -- * WindowSource
    WindowSource(..),

    -- ** Event types
    MouseClick(..),
    KeyPress(..),

    ScanCode(..),
    ModKey(..),

    -- ** WindowEvents
    WindowEvents,
    windowEvents,
    eventsFromHandlers,

    -- ** WindowHandlers
    WindowHandlers,
    windowHandlers,

    -- * Advanced transformations
    combineWindows,
    transWindowSource,
    traverseWindowSource,
)
where

import Control.Monad
import Data.Functor.Identity
import Data.Function ( on )

import Graphics.UI.GLFW as GLFW
import Reactive.Banana hiding ( Identity )
import Reactive.Banana.Frameworks

import Reactive.Banana.GLFW.AddHandler


newtype ScanCode = SC Int
    deriving (Show, Read, Eq, Ord)

data KeyPress = KeyPress !Key !ScanCode !KeyState ![ModKey]
    deriving (Show, Eq, Ord)

data MouseClick = MouseClick !MouseButton !MouseButtonState ![ModKey]
    deriving (Show, Eq, Ord)


data ModKey = Shift | Ctl | Alt | Super
    deriving (Show, Read, Ord, Eq, Bounded, Enum)


listModKeys :: ModifierKeys -> [ModKey]
listModKeys (ModifierKeys sh c a s) = map snd $ filter fst
    [ (sh, Shift)
    , (c,  Ctl)
    , (a,  Alt)
    , (s,  Super)
    ]


-- | A collection of event sources for a `GLFW.Window`.
data WindowSource f = WindowSource
    { refresh     :: f ()
    , close       :: f ()
    , focus       :: f Bool
    , iconify     :: f Bool
    , position    :: f (Int, Int)
    , size        :: f (Int, Int)
    , key         :: f KeyPress
    , char        :: f Char
    , mouse       :: f MouseClick
    , cursorPos   :: f (Double, Double)
    , cursorEnter :: f Bool
    }


type WindowEvents t = WindowSource (Event t)

-- | Obtain a `WindowEvents` for a `GLFW.Window`.
--
-- This will register every callback associated with this Window.
-- If you need to register additional callbacks outside of reactive-banana,
-- see `windowHandlers`.
--
windowEvents :: (Frameworks t) => GLFW.Window -> Moment t (WindowEvents t)
windowEvents = eventsFromHandlers <=< liftIO . windowHandlers


type WindowHandlers = WindowSource AddHandler'

-- | Obtain a `WindowHandlers` for a `GLFW.Window`.
--
-- You only need a `WindowHandlers` if you need to register callbacks
-- outside of reactive-banana. Otherwise, use `windowEvents`.
--
-- Use `registerCallback` on the components of the `WindowHandlers` for
-- traditional `IO` callbacks. Then use `eventsFromHandlers` to obtain a
-- `WindowEvents` to use reactive-banana as well.
--
-- Note that this will register every callback associated with this Window.
-- If a callback is registered directly with GLFW instead of this
-- `WindowHandlers`, it will stop working for this `WindowHandlers`.
--
windowHandlers :: GLFW.Window -> IO WindowHandlers
windowHandlers w = WindowSource
    <$> hc1 (GLFW.setWindowRefreshCallback w)
    <*> hc1 (GLFW.setWindowCloseCallback w)
    <*> (fmap (== FocusState'Focused)
        <$> hc2 (GLFW.setWindowFocusCallback w))
    <*> (fmap (== IconifyState'Iconified)
        <$> hc2 (GLFW.setWindowIconifyCallback w))
    <*> hc3 (GLFW.setWindowPosCallback w)
    <*> hc3 (GLFW.setWindowSizeCallback w)
    <*> handleCallback (\f _ k i s m -> f $ KeyPress k (SC i) s (listModKeys m))
            (GLFW.setKeyCallback w)
    <*> hc2 (GLFW.setCharCallback w)
    <*> handleCallback (\f _ mb s m -> f $ MouseClick mb s (listModKeys m))
            (GLFW.setMouseButtonCallback w)
    <*> hc3 (GLFW.setCursorPosCallback w)
    <*> (fmap (== CursorState'InWindow)
        <$> hc2 (GLFW.setCursorEnterCallback w))
  where
    hc1 = handleCallback $ \fire _ -> fire ()
    hc2 = handleCallback $ \fire _ a -> fire a
    hc3 = handleCallback $ \fire _ a b -> fire (a, b)



-- | Obtain a `WindowEvents` from a `WindowHandlers`.
eventsFromHandlers :: Frameworks t => WindowHandlers -> Moment t (WindowEvents t)
eventsFromHandlers = traverseWindowSource fromAddHandler'



-- | Create an `AddHandler'` for a callback in the shape provided by GLFW-b
handleCallback
    :: ((a -> IO ()) -> cb)
    -> (Maybe cb -> IO ())
    -> IO (AddHandler' a)
handleCallback f cb = do
    (ah, fire) <- newAddHandler
    liftIO $ cb $ Just $ f fire
    return $ AddHandler' ah


-- this is essentially a monomorphic definition of:
--   instance Monoid1 (WindowSource f)

-- | @combineWindows f a b@ produces a `Window` with components defined by
-- @f ca cb@ for each component @ca@ in @a@ and @cb@ in @b@.
combineWindows
    :: (forall a. f a -> f a -> f a)
    -> WindowSource f
    -> WindowSource f
    -> WindowSource f
combineWindows f w0 w1 = WindowSource
    { refresh     = combine refresh
    , close       = combine close
    , focus       = combine focus
    , iconify     = combine iconify
    , position    = combine position
    , size        = combine size
    , key         = combine key
    , char        = combine char
    , mouse       = combine mouse
    , cursorPos   = combine cursorPos
    , cursorEnter = combine cursorEnter
    }
  where
    combine r = on f r w0 w1


-- | @transWindowSource n w@ is the `WindowSource` resulting from applying
-- the natural transformation @n@ to each component of @w@.
transWindowSource :: (forall a. f a -> g a) -> WindowSource f -> WindowSource g
transWindowSource f = runIdentity . traverseWindowSource (Identity . f)

-- | @traversWindowSource
traverseWindowSource
    :: (Applicative m)
    => (forall a. f a -> m (g a))
    -> WindowSource f
    -> m (WindowSource g)
traverseWindowSource f w = WindowSource
    <$> f (refresh w)
    <*> f (close w)
    <*> f (focus w)
    <*> f (iconify w)
    <*> f (position w)
    <*> f (size w)
    <*> f (key w)
    <*> f (char w)
    <*> f (mouse w)
    <*> f (cursorPos w)
    <*> f (cursorEnter w)
