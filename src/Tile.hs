module Tile where

import Graphics
import Math.Utils


data Tile = Tile GLfloat GLfloat GLfloat

setTile :: Tile -> IO ()
setTile (Tile r g b) = color3 r g b

simpleTiles :: Int -> Tile
simpleTiles 0 = Tile 0 0 0       -- floor
simpleTiles 1 = Tile 0.7 0.7 0.7 -- stone
simpleTiles 2 = Tile 0.7 0.4 0.4 -- iron
