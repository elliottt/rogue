module Main where

import Control.Concurrent (threadDelay)
import qualified SDL

main :: IO ()
main  = do
  SDL.init SDL.initVideo
  threadDelay 1000000
  SDL.quit
