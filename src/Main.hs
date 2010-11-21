module Main where

import CaveMan
import Event
import Graphics
import Math.Utils
import Tile
import Time

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Exit (exitSuccess)

data World = World
  { movingUp, movingDown, turningLeft, turningRight :: !Bool
  }

emptyWorld = World
  { movingUp     = False
  , movingDown   = False
  , turningLeft  = False
  , turningRight = False
  }

applyWorld :: World -> CaveMan -> IO ()
applyWorld w cm = do
  when (movingUp     w) (movePlayer moveForward  cm)
  when (movingDown   w) (movePlayer moveBackward cm)
  when (turningLeft  w) (movePlayer rotLeft      cm)
  when (turningRight w) (movePlayer rotRight     cm)

modifyWorld :: TVar World -> (World -> World) -> IO ()
modifyWorld var k = atomically $ do
  w <- readTVar var
  writeTVar var $! k w

setMovingUp, setMovingDown, setTurningLeft, setTurningRight
  :: Bool -> World -> World

setMovingUp b w = w { movingUp = b }
setMovingDown b w = w { movingDown = b }
setTurningLeft b w = w { turningLeft = b }
setTurningRight b w = w { turningRight = b }

main :: IO ()
main  = do
  initGraphics "Rogue" 640 480

  cm     <- initCaves
  world  <- atomically (newTVar emptyWorld)

  withEventManager $ \ win -> do

    win `listen` \ QuitEvent -> exitSuccess

    win `listen` \ (KeyDown sym) ->
      case symKey sym of
        SDLK_e  -> putStrLn "E"
        SDLK_q  -> exitSuccess

        SDLK_UP    -> modifyWorld world (setMovingUp     True)
        SDLK_DOWN  -> modifyWorld world (setMovingDown   True)
        SDLK_LEFT  -> modifyWorld world (setTurningLeft  True)
        SDLK_RIGHT -> modifyWorld world (setTurningRight True)

        _       -> return ()

    win `listen` \ (KeyUp sym) ->
      case symKey sym of

        SDLK_UP    -> modifyWorld world (setMovingUp     False)
        SDLK_DOWN  -> modifyWorld world (setMovingDown   False)
        SDLK_LEFT  -> modifyWorld world (setTurningLeft  False)
        SDLK_RIGHT -> modifyWorld world (setTurningRight False)

        _ -> return ()

    let loop d0 last = do
          now <- getTicks
          processEvents win


          let delta          = (now - last) + d0
              d | delta > 50 = 0
                | otherwise  = delta

          when (d == 0) $ withMatrix $ do
            clearScreen
            translate 0 0 (-6 :: GLfloat)
            renderCaves simpleTiles cm
            updateScreen

          loop d now

    _ <- forkIO $ forever $ do
      w <- atomically (readTVar world)
      applyWorld w cm
      threadDelay (10 * 1000)

    loop 0 =<< getTicks
