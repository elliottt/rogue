{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Tiles
import Rogue.State

import           Control.Concurrent (threadDelay)
import qualified Control.Exception as X
import qualified Data.Map as Map
import           Foreign.C.String (withCString)
import qualified Graphics.UI.SDL as SDL


main :: IO ()
-- main  =


main = withSDL $
  do window   <- createWindow
     renderer <- SDL.createRenderer window (-1) 0
     _ <- SDL.setRenderDrawColor renderer 255 0 0 255

     tiles <- loadTiles renderer

     _ <- SDL.renderClear renderer
     drawTile renderer (head (Map.elems tiles)) 10 10 32 32
     SDL.renderPresent renderer
     threadDelay 2000000
     SDL.destroyWindow window


withSDL :: IO () -> IO ()
withSDL  = X.bracket_ (SDL.init SDL.SDL_INIT_VIDEO) SDL.quit


createWindow :: IO SDL.Window
createWindow  =
  withCString "Rogue!" $ \ str ->
  do SDL.createWindow str SDL.SDL_WINDOWPOS_UNDEFINED
                          SDL.SDL_WINDOWPOS_UNDEFINED
                          640 480 0
