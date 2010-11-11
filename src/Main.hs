module Main where

import CaveMan
import Event
import Graphics
import Tile
import Time

import System.Exit (exitSuccess)

main :: IO ()
main  = do
  initGraphics "Rogue" 640 480

  cm <- initCaves

  withEventManager $ \ win -> do

    win `listen` \ QuitEvent -> exitSuccess

    win `listen` \ (KeyDown sym) ->
      case symKey sym of
        SDLK_e  -> putStrLn "E"

        SDLK_UP    -> movePlayer moveNorth cm
        SDLK_DOWN  -> movePlayer moveSouth cm
        SDLK_LEFT  -> movePlayer moveWest  cm
        SDLK_RIGHT -> movePlayer moveEast cm

        _       -> return ()

    win `listen` \ (TickEvent _ _) -> do
      clearScreen
      withMatrix $ do
        translate 0 0 (-10)
        renderCaves simpleTiles cm
        updateScreen

    eventLoop win
