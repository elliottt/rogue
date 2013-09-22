module SDL (

    -- * Basics
    init
  , Init(), initTimer, initVideo, initAudio, initJoystick, initHaptic
          , initNoParachute, initEverything
  , quit

    -- * Video

    -- ** Windowing
  , createWindow, WindowPos(..)
  , WindowFlags(), windowFullscreen, windowOpenGL, windowShown, windowHidden
                 , windowBorderless, windowResizable, windowMinimized
                 , windowMaximized, windowInputGrabbed, windowInputFocus
                 , windowMouseFocus, windowForeign

    -- ** Screen Saver
  , disableScreenSaver, enableScreenSaver
  ) where

import Prelude hiding (init)

import SDL.Init
import SDL.Video
