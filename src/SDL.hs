module SDL (

    -- * Basics
    init, withSDL
  , Init(), initTimer, initVideo, initAudio, initJoystick, initHaptic
          , initNoParachute, initEverything
  , quit

    -- * Video

    -- ** Windowing
  , createWindow, WindowPos(..)
  , destroyWindow
  , WindowFlags(), windowFullscreen, windowOpenGL, windowShown, windowHidden
                 , windowBorderless, windowResizable, windowMinimized
                 , windowMaximized, windowInputGrabbed, windowInputFocus
                 , windowMouseFocus, windowForeign

    -- ** OpenGL
  , GLContext(), gl_createContext, gl_deleteContext

    -- ** Screen Saver
  , disableScreenSaver, enableScreenSaver


    -- * Events
  , pollEvent, Event(..)
  ) where

import Prelude hiding (init)

import SDL.Event
import SDL.Init
import SDL.Video
