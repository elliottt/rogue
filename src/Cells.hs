module Cells where

import Constants
import Math.AffinePlane (Point)

import System.Random (StdGen,randomRs)


-- Cells -----------------------------------------------------------------------

type CellPalette a = Point Int -> Int -> a

newtype Cell = Cell Int
    deriving (Show,Eq)

-- XXX: It would be nice to parameterize over this in the future.
randomCells :: Double -> StdGen -> [Cell]
randomCells r gen = map pickCell (randomRs (0,1) gen)
  where
  pickCell i | i <= r    = rockCell
             | otherwise = floorCell

floorCell :: Cell
floorCell  = Cell 0

rockCell :: Cell
rockCell  = Cell 1

ironCell :: Cell
ironCell  = Cell 2

fmtCell :: CellPalette a -> Point Int -> Cell -> a
fmtCell p pos (Cell i) = p pos i