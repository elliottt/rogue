{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Event (
    -- * Events
    eventLoop
  , Event(..)
  , SomeEvent(..)
  , QuitEvent(..)
  , TickEvent(..)
  , KeyUp(..)
  , KeyDown(..)
  , MouseMotion(..)
  , MouseButtonUp(..)
  , MouseButtonDown(..)

    -- * Event Management
  , EventManager, withEventManager, newEventManager
  , listen, fireEvent
  , HasEvents(..)

    -- * Re-exported Types
  , SDL.Keysym(..)
  , SDL.SDLKey(..)
  , SDL.Modifier(..)
  ) where

import Time

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically,TVar,newTVar,readTVar,writeTVar)
import Data.Foldable (traverse_)
import Data.Int (Int16)
import Data.Typeable (Typeable,cast)
import Data.Word (Word16,Word32)
import Foreign.Ptr (nullPtr)
import qualified Data.Map        as Map
import qualified Graphics.UI.SDL as SDL

type EventHandler = SomeEvent -> IO ()

newtype Ref = Ref Word32
  deriving (Eq,Ord)

type EventManager = TVar EventManager_

data EventManager_ = EventManager_
  { emRefs     :: [Ref]
  , emHandlers :: Map.Map Ref EventHandler
  }

-- | Run some computation with an event manager.
withEventManager :: (EventManager -> IO ()) -> IO ()
withEventManager k = k =<< newEventManager

-- | Create an EventManager.
newEventManager :: IO EventManager
newEventManager  = atomically $ newTVar $ EventManager_
  { emRefs     = map Ref [0 ..]
  , emHandlers = Map.empty
  }

-- | Listen to all events, firing handlers that apply.  This isn't the best way
-- to report events, as it will cause tick events to lag when there are a lot of
-- other events.
eventLoop :: EventManager -> IO ()
eventLoop em = do
  let k :: Event e => e -> IO ()
      k = fireEvent em . toEvent
      loop last = do
        now <- getTicks
        e   <- SDL.pollEvent
        let delta = now - last
        case e of
          SDL.Quit                  -> k QuitEvent
          SDL.KeyUp sym             -> k (KeyUp sym)
          SDL.KeyDown sym           -> k (KeyDown sym)
          SDL.NoEvent               -> k (TickEvent now delta)
          SDL.MouseMotion x y xr yr -> k (MouseMotion x y xr yr)
          SDL.MouseButtonDown x y b -> k (MouseButtonDown x y b)
          SDL.MouseButtonUp x y b   -> k (MouseButtonUp x y b)
          _                         -> return ()
        loop now
  loop =<< getTicks

-- | Fire an event.
fireEvent :: (HasEvents em, Event e) => em -> e -> IO ()
fireEvent em e = do
  let se  = toEvent e
  let var = getEventManager em
  em <- atomically (readTVar var)
  traverse_ (flip id se) (emHandlers em)

-- | Listen to an event.
listen :: (HasEvents em, Event e) => em -> (e -> IO ()) -> IO Ref
listen em k = atomically $ do
  let var = getEventManager em
  let k' se =
        case fromEvent se of
          Just e  -> k e
          Nothing -> return ()
  em <- readTVar var
  case emRefs em of
    []     -> fail "No refs left!"
    r:rest -> do
      writeTVar var em
        { emHandlers = Map.insert r k' (emHandlers em)
        , emRefs     = rest
        }
      return r

-- | Unregister an event handler.
ignore :: HasEvents em => em -> Ref -> IO ()
ignore em r = atomically $ do
  let var = getEventManager em
  em <- readTVar var
  writeTVar var em
    { emHandlers = Map.delete r (emHandlers em)
    , emRefs     = r : emRefs em
    }

class (Show e, Typeable e) => Event e where
  toEvent :: e -> SomeEvent
  toEvent  = SomeEvent

  fromEvent :: SomeEvent -> Maybe e
  fromEvent (SomeEvent e) = cast e

data SomeEvent = forall e. Event e => SomeEvent e
  deriving Typeable

instance Show SomeEvent where
  showsPrec i (SomeEvent e) = parens (showString "SomeEvent " . shows e)
    where
    parens k | i > 0     = showChar '(' . k . showChar ')'
             | otherwise = k

instance Event SomeEvent where
  toEvent   = id
  fromEvent = Just

data QuitEvent = QuitEvent
  deriving (Show,Typeable)

instance Event QuitEvent

data TickEvent = TickEvent !Time !Interval
  deriving (Show,Typeable)

instance Event TickEvent

data KeyUp = KeyUp !SDL.Keysym
  deriving (Show,Typeable)

instance Event KeyUp

data KeyDown = KeyDown !SDL.Keysym
  deriving (Show,Typeable)

instance Event KeyDown

data MouseMotion = MouseMotion !Word16 !Word16 !Int16 !Int16
  deriving (Show,Typeable)

instance Event MouseMotion

data MouseButtonDown = MouseButtonDown !Word16 !Word16 !SDL.MouseButton
  deriving (Show,Typeable)

instance Event MouseButtonDown

data MouseButtonUp = MouseButtonUp !Word16 !Word16 !SDL.MouseButton
  deriving (Show,Typeable)

instance Event MouseButtonUp



class HasEvents a where
  getEventManager :: a -> EventManager

instance HasEvents EventManager where
  getEventManager = id
