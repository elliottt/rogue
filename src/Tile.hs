module Tile where

import Graphics
import Math.AffinePlane
import Math.Utils

import Data.Maybe (fromMaybe)


data Tile = Tile GLfloat GLfloat GLfloat

setTile :: Tile -> IO ()
setTile (Tile r g b) = color3 r g b

simpleTiles :: Point Int -> Int -> Tile
simpleTiles _ 0 = Tile 0.1 0.1 0.1 -- floor
simpleTiles _ 1 = Tile 0.7 0.7 0.7 -- stone
simpleTiles _ 2 = Tile 0.7 0.4 0.4 -- iron

lightStep :: GLfloat
lightStep  = 0.05

type Intensity = Int

lightTile :: Intensity -> Tile -> Tile
lightTile i (Tile r g b) = Tile (r*scale) (g*scale) (b*scale)
  where
  scale = fromIntegral i

{-# INLINE lightedTiles #-}
lightedTiles :: (Point Int -> Intensity) -> Point Int -> Int -> Tile
lightedTiles light p ty = lightTile (light p) (simpleTiles p ty)
