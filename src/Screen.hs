module Screen where

import Cells
import Constants
import Graphics
import Tile

import Control.Monad.Primitive (PrimState)
import qualified Data.Vector.Mutable as MVec


-- Viewable Screen -------------------------------------------------------------

newtype Screen = Screen { screenCells :: MVec.MVector (PrimState IO) Cell }

-- | Allocate a new Screen that is big enough to hold the nine active chunks.
newScreen :: IO Screen
newScreen  = do
  cells <- MVec.replicate screenSize floorCell
  return Screen { screenCells = cells }

screenSize = chunkLen * 9

writeCell :: Screen -> Int -> Cell -> IO ()
writeCell (Screen mv) = MVec.write mv

readCell :: Screen -> Int -> IO Cell
readCell (Screen mv) = MVec.read mv

renderScreen :: CellPalette Tile -> Screen -> IO ()
renderScreen pal screen = withMatrix $ do

  putStrLn "render"
  renderPrimitive Lines $ do
    color3 1 1 1
    vertex2d (-screenX) chunkHeightF
    vertex2d   screenX  chunkHeightF

    vertex2d (-screenX) 0
    vertex2d   screenX  0

    vertex2d 0 (-screenY)
    vertex2d 0   screenY

    vertex2d   chunkWidthF (-screenY)
    vertex2d   chunkWidthF   screenY

  -- render the cells, after translating out based on the center of the map
  let loop i x y
        | i == screenSize = return ()
        | otherwise       = do
          cell <- readCell screen i

          setTile (fmtCell pal cell)
          vertex2d (x+cellSize) (y+cellSize) -- top right
          vertex2d  x           (y+cellSize) -- top left
          vertex2d  x            y           -- bottom left
          vertex2d (x+cellSize)  y           -- bottom right

          let i'             = i + 1
              boundary       = i' `mod` screenWidth == 0
              x' | boundary  = -chunkWidthF
                 | otherwise = x + cellSize
              y' | boundary  = y - cellSize
                 | otherwise = y

          loop i' x' y'

  renderPrimitive Quads (loop 0 (-chunkWidthF) (chunkHeightF * 2))

screenX     = negate (fromIntegral chunkWidth  * cellSize * 1.5)
screenWidth = chunkWidth * 3
screenY     = negate (fromIntegral chunkHeight * cellSize * 1.5)


