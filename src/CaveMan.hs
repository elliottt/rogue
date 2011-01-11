module CaveMan where

import Cells
import Chunk
import Constants
import Graphics
import Lights
import Math.AffinePlane
import Math.Utils
import Screen
import Tile
import Types

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive (PrimState)
import Control.Concurrent
import Control.Concurrent.STM
import Data.List (find)
import Data.Maybe
import System.Random
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Vector         as Vec
import qualified Data.Vector.Mutable as MVec


-- Cave Manager ----------------------------------------------------------------

type Chunks = Map.Map ChunkId ChunkRef

-- Index into the chunk
type Light = (Int,Int)

data CaveMan = CaveMan
  { caveChunks :: TMVar Chunks
  , cavePlayer :: TMVar Player
  , caveScreen :: TMVar Screen
  }

newCaveMan :: IO CaveMan
newCaveMan  =
  CaveMan <$> atomically newEmptyTMVar              -- caves
          <*> atomically newEmptyTMVar              -- player
          <*> (atomically . newTMVar =<< newScreen) -- screen

(=:) :: TMVar a -> a -> IO ()
var =: x = atomically $ do
  _ <- tryTakeTMVar var
  putTMVar var $! x

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
  _ <- loadChunk cm (0,0)
  sequence_ [ loadChunk cm (x,y) | y <- [(-1), 0, 1], x <- [(-1), 0, 1] ]
  blitCaves cm
  return cm

split5 :: StdGen -> (StdGen,StdGen,StdGen,StdGen,StdGen)
split5 gen = (g0,g1,g2,g3,g4)
  where
  (g0,gen1) = split gen
  (g1,gen2) = split gen1
  (g2,gen3) = split gen2
  (g3,g4)   = split gen3

modifyTMVar :: TMVar a -> (a -> IO (a,b)) -> IO b
modifyTMVar var k = do
  a      <- atomically (takeTMVar var)
  (a',b) <- k a
  atomically (putTMVar var a')
  return b


loadChunk :: CaveMan -> ChunkId -> IO Chunk
loadChunk cm gid =
  modifyTMVar (caveChunks cm) $ \ c -> do
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

-- | Blit the active chunks into a screen.
blitCaves :: CaveMan -> IO ()
blitCaves cm = do
  p      <- atomically (readTMVar (cavePlayer cm))
  screen <- atomically (takeTMVar (caveScreen cm))

  -- retrieve the active chunks
  let chunk (off,ix) = blitChunk off screen =<< loadChunk cm ix
  mapM_ chunk (zip chunkOffsets (activeChunks p))

  caveScreen cm =: screen

chunkOffsets :: [Int]
chunkOffsets  = [ offset x y | y <- [0,1,2], x <- [2,1,0] ]
  where
  offset x y = x * chunkWidth + y * chunkHeight * chunkWidth * 3

-- The world point (0,0) is the center of the (0,0) chunk.
renderOrigin :: Player -> IO ()
renderOrigin p = do
  let rads = negate (vectorRads (playerDir p))
  rotate (radsToDegrees rads) 0 0 1

  let Point x y = playerPos p
  translate (negate x) (negate y) 0

radsToDegrees :: GLfloat -> GLfloat
radsToDegrees rads = rads * 180 / pi

vectorRads :: Vector GLfloat -> GLfloat
vectorRads v = atan2 0 1 - atan2 a b
  where
  Vector a b = unitV v

renderCaves :: CellPalette Tile -> CaveMan -> IO ()
renderCaves pal cm = do
  renderPlayer
  p <- atomically (readTMVar (cavePlayer cm))
  renderOrigin p
  screen <- atomically (readTMVar (caveScreen cm))

  let pos = playerScreenPosP p
  cells <- fov screen pos 10
  renderPrimitive Quads (renderScreenPoints screen (Set.toList cells))


-- Player ----------------------------------------------------------------------

data Player = Player
  { playerPos   :: !(Point  GLfloat)
  , playerDir   :: !(Vector GLfloat)
  , playerChunk :: ChunkId
  } deriving Show

renderPlayer :: IO ()
renderPlayer  = renderPrimitive Triangles $ do
  color3 1 1 1
  let vert a b = vertex2d (a :: GLfloat) b
  vert 0 0.1
  vert (-0.05) 0
  vert 0.05 0

emptyPlayer = Player
  { playerPos   = Point 0 0
  , playerDir   = Vector 0 1
  , playerChunk = (0,0)
  }

movePlayer :: Movement -> CaveMan -> IO ()
movePlayer move cm = do
  let var = cavePlayer cm
  p <- atomically (readTMVar var)
  let p' = constrainPos (move p)
  var =: p'
  when (playerChunk p /= playerChunk p') (blitCaves cm)

-- | The offset into the current chunk.
playerChunkPosP :: Player -> Point Int
playerChunkPosP p = Point x' y'
  where
  Point x y = playerPos p
  x'        = floor ((x+chunkWidthF2)  / cellSize) `mod` chunkWidth
  y'        = floor (negate (y+chunkHeightF2) / cellSize) `mod` chunkHeight

-- | The position of the player in the (1,1) chunk
playerChunkPos :: Player -> (Int,Int)
playerChunkPos p = (x,y)
  where
  Point x y = playerChunkPosP p

-- | The player is always in the (1,1) chunk.
playerScreenPosP :: Player -> Point Int
playerScreenPosP p = playerChunkPosP p .+^ Vector chunkWidth chunkHeight

-- | Enumerate the blocks that surround the player, starting from the top-left
-- and ending in the bottom-right.
activeChunks :: Player -> [ChunkId]
activeChunks p = do
  let (x0,y0) = playerChunk p
  y <- [y0+1, y0, y0-1]
  x <- [x0-1, x0, x0+1]
  return (x,y)


-- Player Movement -------------------------------------------------------------

-- | Make sure that the player stays within the active tile, moving their chunk
-- index once they stray outside.
constrainPos :: Player -> Player
constrainPos p = p
  { playerPos   = Point x' y'
  , playerChunk = (cx',cy')
  }
  where
  Point x y            = playerPos p
  (cx,cy)              = playerChunk p
  (cx', x')
    | x >  chunkWidthF2 = (cx-1, -chunkWidthF2)
    | x < -chunkWidthF2 = (cx+1,  chunkWidthF2)
    | otherwise         = (cx, x)
  (cy', y')
    | y >  chunkHeightF2 = (cy+1, -chunkHeightF2)
    | y < -chunkHeightF2 = (cy-1,  chunkHeightF2)
    | otherwise        = (cy,y)

type Movement = Player -> Player

moveForward :: Movement
moveForward p = p
  { playerPos = playerPos p .+^ (playerStep *^ playerDir p)
  }

moveBackward :: Movement
moveBackward p = p
  { playerPos = playerPos p .-^ (playerStep *^ playerDir p)
  }

rotLeft :: Movement
rotLeft p = p { playerDir = Vector x' y' }
  where
  srot        = sin playerRotate
  crot        = cos playerRotate
  Vector x y  = playerDir p
  x'          = crot * x - srot * y
  y'          = srot * x + crot * y

rotRight :: Movement
rotRight p = p { playerDir = Vector x' y' }
  where
  srot        = sin (negate playerRotate)
  crot        = cos (negate playerRotate)
  Vector x y  = playerDir p
  x'          = crot * x - srot * y
  y'          = srot * x + crot * y
