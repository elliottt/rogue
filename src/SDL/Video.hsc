{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}

module SDL.Video where

import Control.Monad ( when )
import Data.Bits ( (.|.) )
import Data.Monoid ( Monoid(..) )
import Data.Typeable ( Typeable )
import Foreign.C.Types ( CUInt(..), CInt(..) )
import Foreign.C.String ( CString, withCString )
import Foreign.Ptr ( FunPtr(..), Ptr(..), nullPtr )
import Foreign.ForeignPtr.Safe
    ( ForeignPtr(), newForeignPtr, finalizeForeignPtr )
import qualified Control.Exception as X

#include <SDL.h>


-- Errors ----------------------------------------------------------------------

data VideoException = CreateWindowFailed
                      deriving (Show,Typeable)

instance X.Exception VideoException


-- Create Window ---------------------------------------------------------------

newtype WindowFlags = WindowFlags { getWindowFlags :: CUInt }

instance Monoid WindowFlags where
  mempty      = WindowFlags 0
  mappend l r = WindowFlags (getWindowFlags l .|. getWindowFlags r)

windowFullscreen :: WindowFlags
windowFullscreen  = WindowFlags (#const SDL_WINDOW_FULLSCREEN)

windowOpenGL :: WindowFlags
windowOpenGL  = WindowFlags (#const SDL_WINDOW_OPENGL)

windowShown :: WindowFlags
windowShown  = WindowFlags (#const SDL_WINDOW_SHOWN)

windowHidden :: WindowFlags
windowHidden  = WindowFlags (#const SDL_WINDOW_HIDDEN)

windowBorderless :: WindowFlags
windowBorderless  = WindowFlags (#const SDL_WINDOW_BORDERLESS)

windowResizable :: WindowFlags
windowResizable  = WindowFlags (#const SDL_WINDOW_RESIZABLE)

windowMinimized :: WindowFlags
windowMinimized  = WindowFlags (#const SDL_WINDOW_MINIMIZED)

windowMaximized :: WindowFlags
windowMaximized  = WindowFlags (#const SDL_WINDOW_MAXIMIZED)

windowInputGrabbed :: WindowFlags
windowInputGrabbed  = WindowFlags (#const SDL_WINDOW_INPUT_GRABBED)

windowInputFocus :: WindowFlags
windowInputFocus  = WindowFlags (#const SDL_WINDOW_INPUT_FOCUS)

windowMouseFocus :: WindowFlags
windowMouseFocus  = WindowFlags (#const SDL_WINDOW_MOUSE_FOCUS)

windowForeign :: WindowFlags
windowForeign  = WindowFlags (#const SDL_WINDOW_FOREIGN)


data WindowPos = WindowPos Int Int
               | WindowPosCentered
               | WindowPosUndefined
                 deriving (Show)

newtype Window = Window { getWindow :: ForeignPtr () }

foreign import ccall unsafe "SDL_CreateWindow"
  c_createWindow :: CString -> CInt -> CInt -> CInt -> CInt -> CUInt
                 -> IO (Ptr ())

-- | Create a window.
createWindow :: String -> WindowPos -> Int -> Int -> WindowFlags -> IO Window
createWindow title pos width height flags = do
  win_ptr <- withCString title $ \ p_title ->
      c_createWindow p_title x y (toEnum width) (toEnum height)
          (getWindowFlags flags)

  when (win_ptr == nullPtr) (X.throwIO CreateWindowFailed)

  Window `fmap` newForeignPtr c_destroyWindowPtr win_ptr
  where
  (x,y) = case pos of
    WindowPos a b      -> (toEnum a, toEnum b)
    WindowPosCentered  -> (#const SDL_WINDOWPOS_CENTERED
                          ,#const SDL_WINDOWPOS_CENTERED)
    WindowPosUndefined -> (#const SDL_WINDOWPOS_UNDEFINED
                          ,#const SDL_WINDOWPOS_UNDEFINED)


-- Destroy Window --------------------------------------------------------------

foreign import ccall unsafe "&SDL_DestroyWindow"
  c_destroyWindowPtr :: FunPtr (Ptr () -> IO ())

destroyWindow :: Window -> IO ()
destroyWindow win = finalizeForeignPtr (getWindow win)


-- Screen Saver ----------------------------------------------------------------

foreign import ccall unsafe "SDL_DisableScreenSaver"
  disableScreenSaver :: IO ()

foreign import ccall unsafe "SDL_EnableScreenSaver"
  enableScreenSaver :: IO ()
