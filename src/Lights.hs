module Lights where

import Cells
import Math.AffinePlane
import Math.Utils
import Screen
import Tile

import Control.Monad (forM_,when)
import Control.Monad.Primitive (PrimState)
import Data.List (nub)
import Data.Maybe (catMaybes)
import qualified Data.Map            as Map
import qualified Data.Vector         as Vec
import qualified Data.Vector.Mutable as MVec

-- | Light sources consist of a radius and strength.  The strength values is
-- decremented as distance from the source grows.
data Light = Light
  { lightRadius   :: !Int
  , lightStrength :: !Int
  } deriving (Show,Eq)

-- | A mapping from position to lights.
type Lights = Map.Map (Int,Int) Light

type Iterations = Int

-- | Cast a ray starting at the point, in the given direction, walking n
-- iterations, returning the intersected cell, if one exists.  Lifetime decays
-- by one for each step taken.
castRay :: Screen -> Point Int -> Vector GLfloat -> Iterations
        -> IO (Maybe (Point Int))
castRay screen p0 dir is = loop (move (fromIntegral `fmap` p0)) is
  where
  move p   = p .+^ dir
  loop p 0 = return Nothing
  loop p i = do
    cell <- readCell screen (pointIndex (fmap round p))
    if cell == floorCell
       then loop (move p) (i-1)
       else return (Just (fmap round p))

fovGranularity :: Int
fovGranularity  = 180

fovIndexes :: Int
fovIndexes  = fovGranularity - 1

fovStep :: GLfloat
fovStep  = 2 * pi / fromIntegral fovGranularity

fovAngles :: Vec.Vector (Vector GLfloat)
fovAngles  = Vec.fromList (map fovAngle (take fovGranularity [0, fovStep ..]))
  where
  fovAngle = rotV (Vector 1 0)

-- | Cast a field of view from the given point, stepping n iterations from that
-- point.
fov :: Screen -> Point Int -> Iterations -> IO [Point Int]
fov screen p0 it = do loop fovIndexes
  where
  loop ix
    | ix < 0    = return []
    | otherwise = do
      let angle = fovAngles Vec.! ix
      mb   <- castRay screen p0 angle it
      rest <- loop (ix - 1)
      case mb of
        Nothing                -> return rest
        Just x | x `elem` rest -> return rest
               | otherwise     -> return (x:rest)
