{-# LANGUAGE RecordWildCards #-}

module Rogue.Level where

import           Rogue.Types

import           Control.Monad (guard)
import           Control.Lens (view)
import qualified Data.Array as Arr
import           Data.Function (on)
import qualified Data.HashSet as HS
import qualified Data.Heap as Heap
import           Data.Maybe (mapMaybe)
import           Data.Traversable (mapAccumL)
import           System.Random (StdGen,split,mkStdGen)


-- Queries ---------------------------------------------------------------------

passable :: Cell -> Bool
passable Floor = True
passable Door  = True
passable _     = False

positionValid :: HasPosition pos => pos -> Level -> Bool
positionValid pos (Level tiles) =
  and [ Arr.inRange (Arr.bounds tiles) p
      , passable (tiles Arr.! p)
      ]
  where
  p = view position pos

index :: Level -> Position -> Cell
index (Level arr) pos = arr Arr.! pos


-- Generation ------------------------------------------------------------------

genLevel :: Int -> Seed -> Level
genLevel depth (Seed w h r) =
     Level (Arr.listArray ((0,0),(w-1,h-1)) (repeat Floor))

seedLevels :: Int -> Int -> Int -> StdGen -> (Levels,StdGen)
seedLevels w h depth g = (Arr.listArray (1,depth) ls, g')
  where
  (g',ls) = mapAccumL step g [1 .. depth]

  step r _ = let (gen,r') = split r
              in (r', Left (Seed w h gen))


-- Path Finding ----------------------------------------------------------------

type Path = [Position]

data Node = Node { nodeParent     :: Maybe Node
                 , nodeState      :: !Position
                 , nodeHeuristic  :: !Double
                 , nodePathWeight :: !Double
                 } deriving (Show)

instance Eq Node where
  (==) = (==) `on` nodeState

instance Ord Node where
  compare = compare `on` nodeMeasure

-- | The A* heuristic value.
nodeMeasure :: Node -> Double
nodeMeasure Node { .. } = nodePathWeight + nodeHeuristic

extractPath :: Node -> Path
extractPath  = go []
  where
  go acc Node { .. } =
    case nodeParent of
      Just node -> go (nodeState : acc) node
      Nothing   -> acc

-- | Find positions between two reachable points.
findPath :: Position -> Position -> Level -> Maybe Path
findPath start end lev = search HS.empty (Heap.singleton root)
  where
  root = Node { nodeParent     = Nothing
              , nodeState      = start
              , nodeHeuristic  = distance start end
              , nodePathWeight = 0 }

  search seen queue =
    case Heap.uncons queue of
      Just (node,rest)
        | nodeState node == end ->
          return (extractPath node)

        | otherwise ->
          let succs = successors lev end seen node
           in search (updateSeen succs seen) (foldr Heap.insert rest succs)

      Nothing ->
          Nothing

  updateSeen nodes = HS.union (HS.fromList (map nodeState nodes))

-- | Generate the successors.
successors :: Level -> Position -> HS.HashSet Position -> Node -> [Node]
successors lev target seen parent =
  mapMaybe moveTo [ North, NorthEast, East, SouthEast, South, SouthWest
                  , West, NorthWest ]
  where

  moveTo dir =
    do let pos = move dir (nodeState parent)
       guard (isValid pos)
       return Node { nodeParent     = p
                   , nodeHeuristic  = distance pos target
                   , nodeState      = pos
                   , nodePathWeight = w
                   }

  isValid pos = not (HS.member pos seen) && positionValid pos lev

  p = Just parent
  w = nodePathWeight parent + 1


-- Heuristics ------------------------------------------------------------------

distance :: Position -> Position -> Double
distance (x1,y1) (x2,y2) =
  sqrt (fromIntegral (x1 - x2) ^ 2 + fromIntegral (y1 - y2) ^ 2)
