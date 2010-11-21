module Chunk where

import Cells
import Constants
import Graphics
import Math.Utils
import Screen

import Control.Monad (when,forM_,replicateM_,zipWithM_)
import Control.Monad.Primitive (PrimState)
import System.Random
import qualified Data.Vector         as Vec
import qualified Data.Vector.Mutable as MVec


-- Chunk Ids -------------------------------------------------------------------

type ChunkId = (Int,Int)

idNorth, idEast, idSouth, idWest :: ChunkId -> ChunkId
idNorth (x,y) = (x,y+1)
idEast  (x,y) = (x+1,y)
idSouth (x,y) = (x,y-1)
idWest  (x,y) = (x-1,y)


-- Chunks ----------------------------------------------------------------------

data Chunk = Chunk
  { chunkSeed  :: StdGen
  , chunkCells :: Vec.MVector (PrimState IO) Cell
  }

emptyChunk :: StdGen -> IO Chunk
emptyChunk gen = do
  cells <- MVec.replicate chunkLen floorCell
  return Chunk
    { chunkSeed  = gen
    , chunkCells = cells
    }

chunkRenderPos :: ChunkId -> (GLfloat,GLfloat)
chunkRenderPos (x,y) = (fromIntegral x * cellSize, fromIntegral y * cellSize)

chunkOrigin :: ChunkId -> IO () -> IO ()
chunkOrigin cid m = withMatrix $ do
  let (x,y) = chunkRenderPos cid
  translate (x*fromIntegral chunkWidth) (y*fromIntegral chunkHeight) 0
  m

blitChunk :: Int -> Screen -> Chunk -> IO ()
blitChunk off screen ch = do
  cells <- Vec.freeze (chunkCells ch)
  let row                = chunkWidth * 2 + 1
  let blitLoop six ix
        | ix == chunkLen = return ()
        | otherwise      = do
          writeCell screen six (cells Vec.! ix)
          let ix'           = ix + 1
              boundary      = ix' `mod` chunkWidth == 0
              six'
                | boundary  = six + row
                | otherwise = six + 1
          blitLoop six' ix'
  blitLoop off 0

simulateChunk :: Chunk -> IO ()
simulateChunk c = do
  randomizeChunk 0.6 c
  replicateM_ 4 (stepChunk c)
  finalizeChunk 0.9 c

randomizeChunk :: Double -> Chunk -> IO ()
randomizeChunk r c =
  zipWithM_ (MVec.write (chunkCells c))
      [0 .. chunkLen - 1]
      (randomCells r (chunkSeed c))

stepChunk :: Chunk -> IO ()
stepChunk c = do
  let cells = chunkCells c
  c0 <- Vec.freeze cells
  forM_ [0 .. chunkLen - 1] $ \ ix -> do
    let ns = neighbors c0 ix
    if length (filter (== rockCell) ns) >= 5
       then MVec.write cells ix rockCell
       else MVec.write cells ix floorCell

finalizeChunk :: Double -> Chunk -> IO ()
finalizeChunk i c = do
  let cells= chunkCells c
  c0 <- Vec.freeze cells
  forM_ [0 .. chunkLen - 1] $ \ ix ->
    when (all (== rockCell) (neighbors c0 ix)) $ do
      r <- randomRIO (0,1)
      when (r >= i) (MVec.write cells ix ironCell)

neighbors :: Vec.Vector Cell -> Int -> [Cell]
neighbors arr ix = neighborIxMap ix (arr Vec.!)

-- | The Moore neighborhood on a single chunk.
neighborIxMap :: Int -> (Int -> a) -> [a]
neighborIxMap i0 f =
  [ f i | i <- row0 ++ row1 ++ row2, p i ]
  where
  p ix = ix >= 0 && ix < chunkLen
  (y0,x0) = i0 `divMod` chunkWidth
  row0    = [i-1, i, i+1]
    where
    i = i0 - chunkWidth
  row1    = [i0-1,i0+1]
  row2    = [i-1, i, i+1]
    where
    i = i0 + chunkWidth
