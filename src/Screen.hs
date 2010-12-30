module Screen where

import Cells
import Constants
import Graphics
import Math.AffinePlane
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

pointIndex :: Point Int -> Int
pointIndex (Point x y) = x + y * screenWidth

renderScreen :: CellPalette Tile -> Screen -> IO ()
renderScreen pal screen = do

  renderPrimitive Lines $ do
    color3 1 1 1
    -- top
    vertex2d (-screenX) chunkHeightF2
    vertex2d   screenX  chunkHeightF2

    -- bottom
    vertex2d (-screenX) (-chunkHeightF2)
    vertex2d   screenX  (-chunkHeightF2)

    -- left
    vertex2d (-chunkWidthF2) (-screenY)
    vertex2d (-chunkWidthF2)   screenY

    -- right
    vertex2d chunkWidthF2 (-screenY)
    vertex2d chunkWidthF2   screenY

  -- render the cells, after translating out based on the center of the map
  let loop i px py x y
        | i == screenSize = return ()
        | otherwise       = do
          cell <- readCell screen i

          setTile (fmtCell pal (Point px py) cell)
          vertex2d (x+cellSize) (y-cellSize) -- top right
          vertex2d  x           (y-cellSize) -- top left
          vertex2d  x            y           -- bottom left
          vertex2d (x+cellSize)  y           -- bottom right

          let i'             = i + 1
              boundary       = i' `mod` screenWidth == 0
              (px',x') | boundary  = (0,-screenX)
                       | otherwise = (px+1,x + cellSize)
              (py',y') | boundary  = (py+1, y - cellSize)
                       | otherwise = (py,y)

          loop i' px' py' x' y'

  renderPrimitive Quads (loop 0 0 0 (negate screenX) screenY)

screenWidth = chunkWidth * 3
screenX     = chunkWidthF * 1.5
screenY     = chunkHeightF * 1.5
