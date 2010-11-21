module Constants where

import Math.Utils

chunkWidth :: Int
chunkWidth  = 64

chunkWidthF :: GLfloat
chunkWidthF  = fromIntegral chunkWidth * cellSize

chunkHeight :: Int
chunkHeight  = 64

chunkHeightF :: GLfloat
chunkHeightF  = fromIntegral chunkHeight * cellSize

chunkLen :: Int
chunkLen  = chunkWidth * chunkHeight

playerStep :: GLfloat
playerStep  = 0.02

playerRotate :: GLfloat
playerRotate  = pi / 90

cellSize :: GLfloat
cellSize  = 0.2
