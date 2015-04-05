module Rogue.Map (

    ChunkGen
  , MapSpec(..)

  , Map()
  , newMap

  , MapView()
  , newMapView

  , MapLike(..)
  ) where

import           Control.Monad ( replicateM )
import           Data.Array.IO ( IOArray, newListArray, range, rangeSize )
import           Data.IORef ( IORef, newIORef, readIORef, modifyIORef' )
import qualified Data.Map as Map


-- Chunks ----------------------------------------------------------------------

type Chunk = IOArray (Int,Int)

newChunk :: MapSpec tile -> IO (Chunk tile)
newChunk ms =
  do tiles <- msChunkGen ms ms
     let chunkGenErr = error "Chunk generator didn't fill the chunk"
     newListArray (chunkBounds ms) (tiles ++ repeat chunkGenErr)


-- Map Specifications ----------------------------------------------------------

type ChunkGen tile = MapSpec tile -> IO [tile]

data MapSpec tile = MapSpec { msChunkWidth
                            , msChunkHeight :: !Int
                            , msChunkGen    :: ChunkGen tile
                            }

chunkBounds :: MapSpec tile -> ( (Int,Int), (Int,Int) )
chunkBounds ms = ( (0,0), (msChunkWidth ms-1, msChunkHeight ms-1) )


-- Maps ------------------------------------------------------------------------

-- | Chunk ids specify the coordinate, modulo the chunk size.
type ChunkId = (Int,Int)

data Map tile = Map { mapChunks :: IORef (Map.Map ChunkId (Chunk tile))
                    , mapSpec   :: !(MapSpec tile)
                    }

newMap :: MapSpec tile -> IO (Map tile)
newMap spec =
  do ref <- newIORef Map.empty
     return Map { mapChunks = ref, mapSpec = spec }


-- Map Views -------------------------------------------------------------------

data MapView tile = MapView { mvMap    :: Map tile
                            , mvActive :: IOArray (Int,Int) (Chunk tile)
                            , mvPosX   :: !Int
                            , mvPosY   :: !Int
                            }

-- | Construct a view of a map that contains a buffer of loaded chunks,
-- len x len in size.
newMapView :: Int -> Map tile -> IO (MapView tile)
newMapView len m
  | len <= 0  = fail "Invalid map view dimension"
  | otherwise =
    do let bounds = ((0,0),(len-1, len-1))
       chunks <- mapM (`getChunk` m) (range bounds)
       buffer <- newListArray bounds chunks
       return MapView { mvMap    = m
                      , mvActive = buffer
                      , mvPosX   = 0
                      , mvPosY   = 0 }


-- Map-like Structures ---------------------------------------------------------

class MapLike map where
  getChunk :: ChunkId -> map tile -> IO (Chunk tile)

instance MapLike Map where
  getChunk cid m =
    do chunks <- readIORef (mapChunks m)
       case Map.lookup cid chunks of
         Just c  -> return c
         Nothing -> do chunk <- newChunk (mapSpec m)
                       modifyIORef' (mapChunks m) (Map.insert cid chunk)
                       return chunk

instance MapLike MapView where
  getChunk cid mv =
    do 
