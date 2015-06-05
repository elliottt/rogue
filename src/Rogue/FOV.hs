module Rogue.FOV where


import Rogue.Level
import Rogue.Types

import qualified Data.Set as Set


-- | The position and visibility of tiles within the FOV centered at some
-- position.
type VisibilitySet = Set.Set (Int,Position)

fov :: Level -> Position -> VisibilitySet
fov lev start = undefined
