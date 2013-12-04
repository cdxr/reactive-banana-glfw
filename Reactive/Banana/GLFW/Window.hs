{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Reactive.Banana.GLFW.Window
(
    -- * WindowSource
    WindowSource(..),
    bindWindowSource,
    windowEvents,
    -- ** Event types
    KeyPress(..),
    ScanCode(..),
    MouseClick(..),
    ModKey(..),

    -- * Advanced transformations
    combineWindows,
    transWindowSource,
    traverseWindowSource,
)
where

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


-- | Obtain an `Event` from every `AddHandler'` contained in the
-- `WindowSource`.
--
windowEvents
    :: Frameworks t
    => WindowSource AddHandler'
    -> Moment t (WindowSource (Event t))
windowEvents = traverseWindowSource fromAddHandler'


-- | Create a new `AddHandler'` for every window-specific callback provided
-- by GLFW.
bindWindowSource :: GLFW.Window -> IO (WindowSource AddHandler')
bindWindowSource w = WindowSource
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


-- | Create an `AddHandler'` for a callback in the shape provided by GLFW-b
handleCallback
    :: ((a -> IO ()) -> cb)
    -> (Maybe cb -> IO ())
    -> IO (AddHandler' a)
handleCallback f cb = do
    (ah, fire) <- newAddHandler
    liftIO $ cb $ Just $ f fire
    return $ AddHandler' ah
