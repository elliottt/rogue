module Rogue.Ascii where

import Rogue.Dungeon
import Rogue.World

import System.Random ( newStdGen )


-- Ascii Tiles -----------------------------------------------------------------

newtype Ascii = Ascii Char

fromDungeon :: Dungeon -> Ascii
fromDungeon d = case d of
  Wall  -> wall
  Floor -> empty

putAscii :: Ascii -> IO ()
putAscii (Ascii c) = putChar c

render :: Rect -> World Ascii -> IO ()
render v w = mapM_ row =<< readTiles v w
  where
  row cs = do
    mapM_ putAscii cs
    putChar '\n'

wall :: Ascii
wall  = Ascii '#'

empty :: Ascii
empty  = Ascii ' '


-- Testing ---------------------------------------------------------------------

test = do
  g  <- newStdGen
  w  <- newWorld 32 64 Floor
  g' <- worldGen w g dungeon
  v  <- worldRect w
  print v
  render v =<< mapWorld fromDungeon w
