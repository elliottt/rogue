{-# LANGUAGE ForeignFunctionInterface #-}

module SDL.Event where

import Data.Char ( chr, isAscii )
import Data.Int ( Int32(..) )
import Data.Word ( Word8(..) )
import Foreign.C.Types ( CInt(..), CUInt(..) )
import Foreign.Marshal.Alloc ( allocaBytes )
import Foreign.Ptr ( Ptr(..), plusPtr )
import Foreign.Storable ( peekByteOff )
import Numeric ( showHex )

#include <SDL.h>

data Event = NoEvent
           | Quit
           | WindowEvent WindowEvent
           | KeyEvent KeyEvent
             deriving (Show)

data WindowEvent = WindowEventShown
                 | WindowEventHidden
                 | WindowEventExposed

                 | WindowEventMoved Int32 Int32

                 | WindowEventResized Int Int
                 | WindowEventSizeChanged
                 | WindowEventMinimized
                 | WindowEventMaximized
                 | WindowEventRestored

                 | WindowEventEnter
                 | WindowEventLeave
                 | WindowEventFocusGained
                 | WindowEventFocusLost
                 | WindowEventClose
                   deriving (Show)

data KeyEvent = KeyUp   Bool KeySym
              | KeyDown Bool KeySym
                     deriving (Show)

data KeySym = KeyUnknown
            | KeyChar Char
              deriving (Show)

peekEvent :: Ptr Event -> IO Event
peekEvent ptr = do
  tag <- (#peek SDL_Event, type) ptr
  case tag :: CUInt of
    (#const SDL_QUIT) ->
      return Quit

    (#const SDL_KEYDOWN) ->
      KeyEvent `fmap` peekKeyEvent KeyDown ((#ptr SDL_Event,key) ptr)

    (#const SDL_KEYUP) ->
      KeyEvent `fmap` peekKeyEvent KeyUp ((#ptr SDL_Event,key) ptr)

    (#const SDL_WINDOWEVENT) ->
      peekWindowEvent ((#ptr SDL_Event, window) ptr)

    _ -> do putStrLn ("Not handling: 0x" ++ showHex tag "")
            return NoEvent


peekKeyEvent :: (Bool -> KeySym -> KeyEvent) -> Ptr () -> IO KeyEvent
peekKeyEvent mk ptr = do
  repeated <- peekRepeated ptr
  sym      <- peekKeySym ptr
  return (mk repeated sym)

peekRepeated :: Ptr () -> IO Bool
peekRepeated ptr = do
  repeated <- (#peek SDL_KeyboardEvent,repeat) ptr
  case repeated :: Word8 of
    0 -> return False
    _ -> return True

peekKeySym :: Ptr () -> IO KeySym
peekKeySym ptr = do
  sym <- (#peek SDL_Keysym, sym) ((#ptr SDL_KeyboardEvent, keysym) ptr)

  case sym of
    0 -> return KeyUnknown

    _ | sym < 127 -> let char = chr (fromEnum (sym :: CInt))
                      in return (KeyChar char)

    -- XXX control keys
    _ -> return KeyUnknown

peekWindowEvent :: Ptr () -> IO Event
peekWindowEvent we = do
  event <- (#peek SDL_WindowEvent, event) we
  let evt k = return (WindowEvent k)
  case event :: Word8 of
    (#const SDL_WINDOWEVENT_NONE) ->
      return NoEvent

    (#const SDL_WINDOWEVENT_SHOWN)   -> evt WindowEventShown
    (#const SDL_WINDOWEVENT_HIDDEN)  -> evt WindowEventHidden
    (#const SDL_WINDOWEVENT_EXPOSED) -> evt WindowEventExposed

    (#const SDL_WINDOWEVENT_MOVED)   ->
      do x <- (#peek SDL_WindowEvent, data1) we
         y <- (#peek SDL_WindowEvent, data2) we
         evt (WindowEventMoved x y)

    (#const SDL_WINDOWEVENT_RESIZED) ->
      do x <- (#peek SDL_WindowEvent, data1) we
         y <- (#peek SDL_WindowEvent, data2) we
         evt (WindowEventMoved x y)

    (#const SDL_WINDOWEVENT_SIZE_CHANGED) -> evt WindowEventSizeChanged
    (#const SDL_WINDOWEVENT_MINIMIZED)    -> evt WindowEventMinimized
    (#const SDL_WINDOWEVENT_MAXIMIZED)    -> evt WindowEventMaximized
    (#const SDL_WINDOWEVENT_RESTORED)     -> evt WindowEventRestored
    (#const SDL_WINDOWEVENT_ENTER)        -> evt WindowEventEnter
    (#const SDL_WINDOWEVENT_LEAVE)        -> evt WindowEventLeave
    (#const SDL_WINDOWEVENT_FOCUS_GAINED) -> evt WindowEventFocusGained
    (#const SDL_WINDOWEVENT_FOCUS_LOST)   -> evt WindowEventFocusLost
    (#const SDL_WINDOWEVENT_CLOSE)        -> evt WindowEventClose


foreign import ccall unsafe "SDL_PollEvent"
  c_pollEvent :: Ptr Event -> IO CInt

pollEvent :: IO Event
pollEvent  =
  allocaBytes (#size SDL_Event) $ \ e_ptr -> do
    res <- c_pollEvent e_ptr
    if res == 1
       then peekEvent e_ptr
       else return NoEvent
