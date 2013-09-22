{-# LANGUAGE ForeignFunctionInterface  #-}

module SDL.Init where

import Data.Bits ((.|.))
import Data.Monoid (Monoid(..))
import Foreign.C.Types ( CUInt(..) )

#include <SDL.h>


-- Init ------------------------------------------------------------------------

foreign import ccall unsafe "SDL_Init" c_init :: CUInt -> IO ()

newtype Init = Init { getInit :: CUInt }

instance Monoid Init where
  mempty      = Init 0
  mappend l r = Init (getInit l .|. getInit r)

initTimer :: Init
initTimer  = Init (#const SDL_INIT_TIMER)

initAudio :: Init
initAudio  = Init (#const SDL_INIT_AUDIO)

initVideo :: Init
initVideo  = Init (#const SDL_INIT_VIDEO)

initJoystick :: Init
initJoystick  = Init (#const SDL_INIT_JOYSTICK)

initHaptic :: Init
initHaptic  = Init (#const SDL_INIT_HAPTIC)

initNoParachute :: Init
initNoParachute  = Init (#const SDL_INIT_NOPARACHUTE)

initEverything :: Init
initEverything  = Init (#const SDL_INIT_EVERYTHING)

init :: Init -> IO ()
init ival = c_init (getInit ival)


-- Quit ------------------------------------------------------------------------

foreign import ccall unsafe "SDL_Quit" c_quit :: IO ()

quit :: IO ()
quit  = c_quit
