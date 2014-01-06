module Main where

import           Control.Concurrent ( forkIO )
import           Control.Concurrent.STM
                     ( atomically, TChan, newTChan, tryReadTChan, writeTChan )
import           Control.Monad ( forever, when )
import           Data.Array ( Array, array, (!) )
import           Data.Array.IO
                     ( IOUArray, getBounds, newListArray, readArray
                     , writeArray, inRange )
import           Data.List ( transpose )
import           Data.Maybe ( isJust )
import qualified Graphics.Vty as Vty

main :: IO ()
main  =
  do vty    <- Vty.mkVty
     events <- processEvents vty

     h      <- Vty.terminal_handle
     bounds <- Vty.display_bounds h
     cxt    <- Vty.display_context h bounds

     mainLoop vty h cxt events


-- Main Loop -------------------------------------------------------------------

mainLoop :: Vty.Vty -> Vty.TerminalHandle -> Vty.DisplayHandle
         -> Events -> IO ()
mainLoop vty h cxt events = go =<< initialWorld
  where
  go w =
    do mb <- atomically (tryReadTChan events)

       w' <- maybe (return w) (`processEvent` w) mb

       img <- drawWorld w'
       Vty.output_picture cxt (Vty.pic_for_image img)
       case mb of
         Just Quit -> Vty.shutdown vty
         _         -> go w'


-- Event Handling --------------------------------------------------------------

type Events = TChan Event

data Event = Quit
           | MoveLeft
           | MoveRight
           | MoveUp
           | MoveDown
             deriving (Show)

processEvents :: Vty.Vty -> IO Events
processEvents vty =
  do events <- atomically newTChan
     _ <- forkIO $ forever $
       do evt <- Vty.next_event vty
          case handleEvent evt of
            Just evt' -> atomically (writeTChan events evt')
            Nothing   -> return ()
     return events


handleEvent :: Vty.Event -> Maybe Event
handleEvent evt = case evt of
  Vty.EvKey  Vty.KEsc        [] -> Just Quit
  Vty.EvKey (Vty.KASCII 'h') [] -> Just MoveLeft
  Vty.EvKey (Vty.KASCII 'l') [] -> Just MoveRight
  Vty.EvKey (Vty.KASCII 'k') [] -> Just MoveUp
  Vty.EvKey (Vty.KASCII 'j') [] -> Just MoveDown
  _                             -> Nothing


-- World State -----------------------------------------------------------------

data World = World { wPlayer :: Player
                   , wMap    :: Map
                   }

initialWorld :: IO World
initialWorld  =
  do m <- newMap
     return World { wPlayer = initialPlayer
                  , wMap    = m }

processEvent :: Event -> World -> IO World
processEvent evt w = case evt of
  MoveLeft  -> updatePlayer w p { pX = pX p - 1 }
  MoveRight -> updatePlayer w p { pX = pX p + 1 }
  MoveUp    -> updatePlayer w p { pY = pY p - 1 }
  MoveDown  -> updatePlayer w p { pY = pY p + 1 }
  _         -> return w
  where
  p = wPlayer w

updatePlayer :: World -> Player -> IO World
updatePlayer w p =
  do let pos = (pX p, pY p)
     bounds <- getBounds (wMap w)
     if not (inRange bounds pos)
        then return w
        else do t <- readArray (wMap w) pos
                if passable t
                   then return w { wPlayer = p }
                   else return w



data Player = Player { pX, pY :: !Int
                     } deriving (Show)

initialPlayer :: Player
initialPlayer  = Player { pX = 4, pY = 4 }


type Tile = Int

type Map = IOUArray (Int,Int) Tile

palette :: Array Tile Vty.Image
palette  = array (0, 3)
  [ (0, Vty.char Vty.def_attr ' ')
  , (1, Vty.char Vty.def_attr '#')
  , (2, Vty.char Vty.def_attr '>')
  , (3, Vty.char Vty.def_attr '<') ]

passable :: Tile -> Bool
passable t = t /= 1

newMap :: IO Map
newMap  = newListArray ( (0,0), (9,9) ) $ concat $ transpose
  [ [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
  , [ 1, 0, 0, 0, 3, 0, 0, 0, 0, 1 ]
  , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 1 ]
  , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 1 ]
  , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 1 ]
  , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 1 ]
  , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 1 ]
  , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 1 ]
  , [ 1, 0, 0, 0, 2, 0, 0, 0, 0, 1 ]
  , [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ]



drawWorld :: World -> IO Vty.Image
drawWorld w =
  do (_,(w,h)) <- getBounds m
     Vty.vert_cat `fmap` mapM (drawRow w) [ 0 .. h ]
  where
  p = wPlayer w
  m = wMap w
  drawRow w y = Vty.horiz_cat `fmap` mapM getTile [ 0 .. w ]
    where
    check                          = y == pY p
    getTile x | check && x == pX p = return (Vty.char Vty.def_attr '@')
              | otherwise          = do i <- readArray m (x,y)
                                        return (palette ! i)
