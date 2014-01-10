module Main where

import           Control.Applicative ( (<|>) )
import           Control.Concurrent ( forkIO )
import           Control.Concurrent.STM
                     ( atomically, TChan, newTChan, tryReadTChan, writeTChan )
import           Control.Monad ( forever, when, guard, mplus )
import           Data.Array ( Array, listArray, (!) )
import           Data.Array.IO
                     ( IOUArray, getBounds, newListArray, readArray
                     , writeArray, inRange )
import           Data.Foldable ( fold )
import           Data.List ( transpose )
import qualified Data.Map as Map
import           Data.Maybe ( isJust, mapMaybe, listToMaybe, maybeToList )
import           Data.Monoid ( Monoid(..) )
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
palette  = listArray (0, 3)
  [ Vty.char Vty.def_attr ' '
  , Vty.char Vty.def_attr '#'
  , Vty.char Vty.def_attr '>'
  , Vty.char Vty.def_attr '<' ]

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


type Neighbors = Array (Int,Int) Tile

neighbors :: Map -> (Int,Int) -> IO Neighbors
neighbors m cell@(x,y) =
  do bounds <- getBounds m
     let readCells = do a <- [ x - 1, x, x + 1 ]
                        b <- [ y - 1, y, y + 1 ]
                        let coord = (a,b)
                        if inRange bounds coord
                           then return (readArray m coord)
                           else return (return 0)
     listArray ((0,0),(2,2)) `fmap` sequence readCells


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
              | otherwise          = do ns <- neighbors m (x,y)
                                        return (pickTile ns)


pickTile :: Neighbors -> Vty.Image
pickTile ns = case ns ! (1,1) of
  -- wall special cases
  1 | Just i <- lookupPat (wallQuad ns) walls -> i
  cell                                        -> palette ! cell

wallQuad :: Neighbors -> [Tile]
wallQuad ns = [ ns ! (1,0), ns ! (2,1), ns ! (1,2), ns ! (0,1) ]

walls :: PatTrie Vty.Image
walls  = fold
  [ patCase [ Exact 1, Exact 1, Exact 1, Exact 1 ] (c '#')
  , patCase [ Any,     Exact 1, Any    , Exact 1 ] (c '─')
  , patCase [ Exact 1, Any,     Exact 1          ] (c '│')

  , patCase [ Exact 1, Exact 1                   ] (c '└')
  , patCase [ Any    , Exact 1, Exact 1          ] (c '┌')
  , patCase [ Any    , Any    , Exact 1, Exact 1 ] (c '┐')
  , patCase [ Exact 1, Any    , Any    , Exact 1 ] (c '┘')

  , patCase [                                    ] (c '+')
  ]
  where
  c = Vty.char Vty.def_attr


data Pat = Exact Tile | Any deriving (Show,Eq,Ord)

data PatTrie a = PatTrie (Map.Map Pat (PatTrie a)) (Maybe a) deriving (Show)

instance Monoid (PatTrie a) where
  mempty                                = PatTrie Map.empty Nothing
  mappend (PatTrie as a) (PatTrie bs b) =
    PatTrie (Map.unionWith mappend as bs) (a <|> b)

patCase :: [Pat] -> a -> PatTrie a
patCase key i = go key
  where
  go ps = case ps of
    p:rest -> PatTrie (Map.singleton p (go rest)) Nothing
    []     -> PatTrie Map.empty (Just i)

lookupPat :: [Tile] -> PatTrie a -> Maybe a
lookupPat ps pats = listToMaybe (patChoices ps pats)

patChoices :: [Tile] -> PatTrie a -> [a]
patChoices ps (PatTrie m mb) = case ps of

  p:rest ->
    case Map.lookup (Exact p) m `mplus` Map.lookup Any m of
      Just pats -> patChoices rest pats ++ maybeToList mb
      Nothing   -> maybeToList mb

  [] -> maybeToList mb
