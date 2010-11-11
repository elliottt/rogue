module CaveMan where

import Graphics
import Math.Utils
import Tile

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive (PrimState)
import Control.Concurrent
import System.Random
import qualified Data.Map            as Map
import qualified Data.Vector         as Vec
import qualified Data.Vector.Mutable as MVec


-- Cave Manager ----------------------------------------------------------------

type Chunks = Map.Map ChunkId ChunkRef

data CaveMan = CaveMan
  { caveChunks :: MVar Chunks
  , cavePlayer :: MVar Player
  }

newCaveMan :: IO CaveMan
newCaveMan  =  CaveMan
           <$> newEmptyMVar -- caves
           <*> newEmptyMVar -- player

(=:) :: MVar a -> a -> IO ()
var =: x = do
  _ <- tryTakeMVar var
  putMVar var $! x

data ChunkRef
  = ChunkRef  FilePath
  | ChunkSeed StdGen
  | ChunkVal  Chunk

initCaves :: IO CaveMan
initCaves  = do
  var <- newMVar Map.empty
  cm  <- newCaveMan
  caveChunks cm =: Map.empty
  cavePlayer cm =: emptyPlayer
  _ <- loadChunk (0,0) cm
  sequence_ [ loadChunk (x,y) cm | y <- [(-1), 0, 1], x <- [(-1), 0, 1] ]
  return cm

split5 :: StdGen -> (StdGen,StdGen,StdGen,StdGen,StdGen)
split5 gen = (g0,g1,g2,g3,g4)
  where
  (g0,gen1) = split gen
  (g1,gen2) = split gen1
  (g2,gen3) = split gen2
  (g3,g4)   = split gen3

loadChunk :: ChunkId -> CaveMan -> IO Chunk
loadChunk gid cm =
  modifyMVar (caveChunks cm) $ \ c -> do
    case Map.lookup gid c of
      Just (ChunkSeed gen) -> body c gen
      Just (ChunkRef path) -> undefined
      Just (ChunkVal ch)   -> return (c,ch)
      Nothing              -> body c =<< newStdGen
    where
    body c gen = do
      let (g0,g1,g2,g3,g4) = split5 gen
      c0  <- emptyChunk g0
      simulateChunk c0
      let c' = Map.insert gid (ChunkVal c0)
             $ seedChunk (idNorth gid) g1
             $ seedChunk (idEast  gid) g2
             $ seedChunk (idSouth gid) g3
             $ seedChunk (idWest  gid) g4 c
      c' `seq` return (c',c0)

seedChunk :: ChunkId -> StdGen -> Chunks -> Chunks
seedChunk gid gen c
  | Map.member gid c = c
  | otherwise        = Map.insert gid (ChunkSeed gen) c

renderCaves :: CellPalette Tile -> CaveMan -> IO ()
renderCaves pal cm = do

  p <- readMVar (cavePlayer cm)
  renderPlayer

  renderOrigin p
  print (playerChunk p,p)

  -- retrieve the active chunks
  let chunk pos ix = do
        ch <- loadChunk ix cm
        renderChunk pal pos ch
  zipWithM_ chunk chunkGrid (activeChunks p)

chunkGrid = [ (x,y) | y <- [1,0,-1] , x <- [-1,0,1] ]


renderOrigin :: Player -> IO ()
renderOrigin p = do
  translate (negate (playerX p)) (negate (playerY p)) 0
  rotate (playerRot p) 0 0 1


-- Player ----------------------------------------------------------------------

data Player = Player
  { playerX   :: !GLfloat
  , playerY   :: !GLfloat
  , playerRot :: !GLfloat
  } deriving Show

renderPlayer :: IO ()
renderPlayer  = renderPrimitive Triangles $ do
  color3 1 1 1
  vertex2d 0 0.1
  vertex2d (-0.05) 0
  vertex2d 0 0.05

emptyPlayer = Player
  { playerX   = 0
  , playerY   = 0
  , playerRot = 0
  }

movePlayer :: Movement -> CaveMan -> IO ()
movePlayer move cm = do
  let var = cavePlayer cm
  p <- readMVar var
  var =: move p

-- | Given a player, decide what chunk they're contained within.
playerChunk :: Player -> ChunkId
playerChunk p = (x `div` chunkWidth, y `div` chunkHeight)
  where
  x = ceiling (playerX p / cellSize)
  y = ceiling (playerY p / cellSize)

-- | Enumerate the blocks that surround the player, starting from the top-left
-- and ending in the bottom-right.
activeChunks :: Player -> [ChunkId]
activeChunks p = do
  let (x0,y0) = playerChunk p
  y <- [y0+1, y0, y0-1]
  x <- [x0-1, x0, x0+1]
  return (x,y)


-- Player Movement -------------------------------------------------------------

playerIncrement :: GLfloat
playerIncrement  = 1

type Movement = Player -> Player

moveForward :: Movement
moveForward p = p { playerY = playerY p + playerIncrement }

moveBackward :: Movement
moveBackward p = p { playerY = playerY p - playerIncrement }

rotLeft :: Movement
rotLeft p = p { playerRot = playerRot p - 1 }

rotRight :: Movement
rotRight p = p { playerRot = playerRot p + 1 }


-- Chunk Ids -------------------------------------------------------------------

type ChunkId = (Int,Int)

idNorth, idEast, idSouth, idWest :: ChunkId -> ChunkId
idNorth (x,y) = (x,y+1)
idEast  (x,y) = (x+1,y)
idSouth (x,y) = (x,y-1)
idWest  (x,y) = (x-1,y)


-- Chunks ----------------------------------------------------------------------

chunkWidth, chunkHeight, chunkLen :: Int
chunkWidth  = 128
chunkHeight = 128
chunkLen    = chunkWidth * chunkHeight

type Bounds a = (a,a)

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

cellSize :: GLfloat
cellSize  = 0.2

chunkRenderPos :: ChunkId -> (GLfloat,GLfloat)
chunkRenderPos (x,y) = (fromIntegral x * cellSize, fromIntegral y * cellSize)

chunkOrigin :: ChunkId -> IO () -> IO ()
chunkOrigin cid m = withMatrix $ do
  let (x,y) = chunkRenderPos cid
  translate (x*fromIntegral chunkWidth) (y*fromIntegral chunkHeight) 0
  m

renderChunk :: CellPalette Tile -> ChunkId -> Chunk -> IO ()
renderChunk pal ix0 ch = chunkOrigin ix0 $ renderPrimitive Quads $ do
  cells <- Vec.freeze (chunkCells ch)
  let renderLoop ix x y
        | ix >= chunkLen = return ()
        | otherwise      = do

          setTile (fmtCell pal (cells Vec.! ix))
          vertex2d (x+cellSize) (y+cellSize) -- top right
          vertex2d  x           (y+cellSize) -- top left
          vertex2d  x            y           -- bottom left
          vertex2d (x+cellSize)  y           -- bottom right

          let ix'            = ix + 1
              boundary       = ix' `mod` chunkWidth == 0
              x' | boundary  = 0
                 | otherwise = x + cellSize
              y' | boundary  = y + cellSize
                 | otherwise = y

          renderLoop ix' x' y'
  renderLoop 0 0 0

simulateChunk :: Chunk -> IO ()
simulateChunk c = do
  randomizeChunk 0.55 c
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


-- Cells -----------------------------------------------------------------------

type CellPalette a = Int -> a

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

fmtCell :: CellPalette a -> Cell -> a
fmtCell p (Cell i) = p i

simplePalette :: CellPalette Char
simplePalette 0 = ' '
simplePalette 1 = ';'
simplePalette 2 = 'I'
