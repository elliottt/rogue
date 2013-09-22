{-# LANGUAGE ForeignFunctionInterface #-}

module SDL.Event where

import Data.Int ( Int32(..) )
import Data.Word ( Word8(..) )
import Foreign.C.Types ( CInt(..), CUInt(..) )
import Foreign.Marshal.Alloc ( allocaBytes )
import Foreign.Ptr ( Ptr(..), plusPtr )
import Foreign.Storable ( peekByteOff )
import Numeric ( showHex )

#include <SDL.h>

data Event = Quit
           | WindowEvent WindowEvent
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

data KeyboardEvent = KeyboardEvent
                     deriving (Show)

peekEvent :: Ptr Event -> IO (Maybe Event)
peekEvent ptr = do
  tag <- (#peek SDL_Event, type) ptr
  case tag :: CUInt of
    (#const SDL_QUIT) ->
      return (Just Quit)

    (#const SDL_WINDOWEVENT) -> do
      let we = (#ptr SDL_Event, window) ptr
      event <- (#peek SDL_WindowEvent, event) we

      let evt k = return (Just (WindowEvent k))

      case event :: Word8 of
        (#const SDL_WINDOWEVENT_NONE) ->
          return Nothing

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

    _ -> do putStrLn ("Not handling: 0x" ++ showHex tag "")
            return Nothing


foreign import ccall unsafe "SDL_PollEvent"
  c_pollEvent :: Ptr Event -> IO CInt

pollEvent :: IO (Maybe Event)
pollEvent  =
  allocaBytes (#size SDL_Event) $ \ e_ptr -> do
    res <- c_pollEvent e_ptr
    if res == 1
       then peekEvent e_ptr
       else return Nothing
