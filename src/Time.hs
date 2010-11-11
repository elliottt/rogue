module Time (
    module Time
  , SDL.getTicks
  ) where

import Data.Word (Word32)
import qualified Graphics.UI.SDL as SDL

-- | Time, in miliseconds since the engine started.
type Time = Word32

-- | An interval, specified in miliseconds.
type Interval = Word32

addInterval :: Interval -> Time -> Time
addInterval  = (+)

class Update a where
  update :: Time -> a -> IO ()

time :: IO a -> IO a
time m = do
  start <- SDL.getTicks
  res   <- m
  stop  <- SDL.getTicks
  putStrLn ("Elapsed: " ++ show (stop - start) ++ "ms")
  return res
