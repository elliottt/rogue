module Screen where

import Cells
import Constants
import Graphics
import Math.AffinePlane
import Math.Utils
import Tile
import Types

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
              (py',y') | boundary  = (py+1,y - cellSize)
                       | otherwise = (py,y)

          loop i' px' py' x' y'

  renderPrimitive Quads (loop 0 0 0 (negate screenX) screenY)

-- XXX this could sort the points, and use an accumulator for position
renderScreenPoints :: Screen -> [Lighted (Point Int)] -> IO ()
renderScreenPoints screen = mapM_ (renderScreenPoint screen)

pointRenderPos :: Point Int -> (GLfloat,GLfloat)
pointRenderPos (Point x y) = (x',y')
  where
  x' = fromIntegral x * cellSize - screenX
  y' = screenY - fromIntegral y * cellSize

renderScreenPoint :: Screen -> Lighted (Point Int) -> IO ()
renderScreenPoint screen l = do
  let p     = lightedData l
      (x,y) = pointRenderPos p
  cell <- readCell screen (pointIndex p)
  setTile (fmtCell simpleTiles p cell)
  vertex2d (x+cellSize) (y-cellSize) -- top right
  vertex2d  x           (y-cellSize) -- top left
  vertex2d  x            y           -- bottom left
  vertex2d (x+cellSize)  y           -- bottom right


screenWidth = chunkWidth * 3
screenX     = chunkWidthF * 1.5
screenY     = chunkHeightF * 1.5
