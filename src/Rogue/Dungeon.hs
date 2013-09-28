module Rogue.Dungeon where

import Rogue.World

import Control.Monad


data Dungeon = Wall
             | Floor
               deriving (Show,Eq)

dungeon :: WorldGen Dungeon
dungeon  = do

  -- randomize the world
  everywhere $ weight [ (1, replace Wall)
                      , (2, replace Floor)
                      ]

  -- generate some caverns
  replicateM_ 4 $ everywhere $ do
    ns <- neighborhood
    t  <- self

    let walls = length (filter ((Wall ==) . locValue) ns)

    when (t == Floor && walls >= 5)
        (weight [ (4, replace Wall)
                , (1, replace Floor)
                ])

  everywhere $ do
    ns <- neighborhood
    t  <- self

    let walls = length (filter ((Wall ==) . locValue) ns)

    when (t == Wall && walls <= 1) (replace Floor)
