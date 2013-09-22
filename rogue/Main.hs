module Main where

import qualified SDL

import Control.Concurrent (threadDelay)
import Data.Monoid
import qualified Control.Exception as X

main :: IO ()
main  = body `X.finally` SDL.quit
  where
  body = do
    putStrLn "INIT"
    SDL.init SDL.initVideo

    putStrLn "VIDEO"
    win <- SDL.createWindow "rogue" (SDL.WindowPos 0 0) 640 480 mempty

    putStrLn "DELAY"
    threadDelay 5000000
