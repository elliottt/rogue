module Main where

import qualified SDL

import Control.Concurrent (threadDelay)
import Data.Monoid

main :: IO ()
main  = SDL.withSDL SDL.initVideo $ do

  putStrLn "VIDEO"
  win <- SDL.createWindow "rogue" (SDL.WindowPos 0 0) 640 480 SDL.windowOpenGL
  cxt <- SDL.gl_createContext win

  putStrLn "DELAY"
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
