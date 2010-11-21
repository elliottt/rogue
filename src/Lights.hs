module Lights where

import Cells
import Math.AffinePlane
import Screen

import Control.Monad.Primitive (PrimState)
import qualified Data.Map            as Map
import qualified Data.Vector.Mutable as MVec

-- | Light sources consist of a radius and strength.  The strength values is
-- decremented as distance from the source grows.
data Light = Light
  { lightRadius   :: !Int
  , lightStrength :: !Int
  } deriving (Show,Eq)

-- | A mapping from position to lights.
type Lights = Map.Map (Int,Int) Light

castRay :: MVec.MVector (PrimState IO) Cell
castRay  = undefined
