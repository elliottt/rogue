module Main where

import qualified SDL
import qualified Rogue

import Control.Concurrent (threadDelay)

main :: IO ()
main  = Rogue.test

{-
main :: IO ()
main  = SDL.withSDL SDL.initVideo $ do

  win <- SDL.createWindow "rogue" (SDL.WindowPos 0 0) 640 480 SDL.windowOpenGL
  cxt <- SDL.gl_createContext win

  let loop = do evt <- SDL.pollEvent
                case evt of
                  SDL.Quit ->
                    return ()

                  SDL.KeyEvent ke ->
                    print ke >> loop

                  SDL.WindowEvent e ->
                    print e >> loop

                  _ -> threadDelay 1000 >> loop

  loop

  SDL.destroyWindow win
  -}
