{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rogue.World where

import Control.Applicative ( Applicative(..) )
import Data.Array.IO
import Data.Monoid ( Monoid(..) )
import MonadLib
    ( runM, BaseM(..), StateT, get, set, ReaderT, runReaderT, ask
    , WriterT, runWriterT, put )
import System.Random ( StdGen, Random(..) )


-- World Map -------------------------------------------------------------------

newtype World a = World { getWorld :: IOArray Position a }

newWorld :: Int -> Int -> a -> IO (World a)
newWorld w h z = World `fmap` newArray ((-w', -h'), (w',h')) z
  where
  (wq,wr) = w `quotRem` 2
  (hq,hr) = h `quotRem` 2

  w' = wq + wr
  h' = hq + hr

mapWorld :: (a -> b) -> World a -> IO (World b)
mapWorld f w = World `fmap` mapArray f (getWorld w)

writeTile :: World a -> Position -> a -> IO ()
writeTile w = writeArray (getWorld w)

readTile :: World a -> Position -> IO a
readTile w = readArray (getWorld w)

readTiles :: Rect -> World a -> IO [[a]]
readTiles v w = do
  bounds <- worldBounds w

  let ys    = [ rectY v .. rectY v + rectH v ]
      row x = sequence [ readTile w ix | y <- ys
                                       , let ix = (x,y)
                                       , inRange bounds ix ]

  mapM row [ rectX v .. rectX v + rectW v ]

worldBounds :: World a -> IO (Position,Position)
worldBounds w = getBounds (getWorld w)

worldRect :: World a -> IO Rect
worldRect w = do
  ((x,y),(x',y')) <- worldBounds w
  return Rect { rectX = x
              , rectY = y
              , rectW = x' - x
              , rectH = y' - y
              }


-- World Generation ------------------------------------------------------------

newtype Gen t a = Gen
  { getGen :: ReaderT (World t) (StateT StdGen IO) a
  } deriving (Functor,Applicative,Monad)

instance BaseM (Gen t) (Gen t) where
  inBase = id

type WorldGen t = Gen t ()

io :: IO a -> Gen t a
io m = Gen (inBase m)

worldGen :: World t -> StdGen -> WorldGen t -> IO StdGen
worldGen w r m = snd `fmap` runM (getGen m) w r

lookupTiles :: [Position] -> Gen t [Located t]
lookupTiles ixs = Gen $ do
  w <- ask
  inBase $ do
    bounds <- worldBounds w
    let step ix = do t <- readTile w ix
                     return Located { locPos = ix, locValue = t }
    sequence [ step ix | ix <- ixs, inRange bounds ix ]

newtype Tile t a = Tile
  { getTile :: WriterT (Write t) (ReaderT Position (Gen t)) a
  } deriving (Functor,Applicative,Monad)

data Write t = Write t | Keep

instance Monoid (Write t) where
  mempty            = Keep
  mappend Keep r    = r
  mappend l    Keep = l
  mappend _    r    = r

-- | Apply a tile generator at one world position.  This assumes that the
-- position is valid.
at :: Position -> Tile t () -> WorldGen t
at i m = do
  (_,w) <- runReaderT i (runWriterT (getTile m))
  case w of
    Write t -> Gen $ do w <- ask
                        inBase (writeTile w i t)
    _       -> return ()

-- | Apply a tile generator at all the valid positions of a rectangle
with :: Rect -> Tile t () -> WorldGen t
with r m = do
  ixs <- Gen $ do
    w   <- ask
    inBase $ do bounds <- worldBounds w
                return [ filter (inRange bounds) ys | ys <- rectPositions r ]
  mapM_ (mapM_ (`at` m)) ixs


-- | Apply a tile generator at every world position.
everywhere :: Tile t () -> WorldGen t
everywhere m = do
  ixs <- Gen $ do w <- ask
                  range `fmap` inBase (worldBounds w)
  mapM_ (`at` m) ixs

-- | Retrieve the neighborhood for this tile.
neighborhood :: Tile t [Located t]
neighborhood  = Tile $ do
  origin <- ask
  inBase (lookupTiles (posNeighborhood origin))

self :: Tile t t
self  = Tile $ do
  pos <- ask
  inBase $ Gen $ do
    w <- ask
    inBase (readTile w pos)

replace :: t -> Tile t ()
replace t = Tile (put (Write t))


-- Random Values ---------------------------------------------------------------

class Monad m => RandM m where
  liftR :: Random a => (StdGen -> (a,StdGen)) -> m a

instance RandM (Gen t) where
  liftR f = Gen $ do
    g <- get
    let (a,g') = f g
    set g'
    return a

instance RandM (Tile t) where
  liftR f = Tile (inBase (liftR f))

choose :: (RandM m, Random a) => (a,a) -> m a
choose r = liftR (randomR r)

pick :: (RandM m, Random a) => m a
pick  = liftR random

weight :: RandM m => [(Int,m a)] -> m a
weight ms
  | null ms   = fail "weight: Invalid list"
  | otherwise = do
    n <- choose (1,tot)
    go n ms
  where
  -- NOTE: this is just frequency from quickcheck
  tot = sum (map fst ms)

  go n xs = case xs of

    (k,m) : rest | n <= k    -> m
                 | otherwise -> go (n - k) rest

    _ -> fail "weight: Invalid list"


-- World Position --------------------------------------------------------------

type Position = (Int,Int)

posNeighborhood :: Position -> [Position]
posNeighborhood (a,b) = [ (x,y) | x <- [a - 1, a, a + 1]
                                , y <- [b - 1, b, b + 1]
                                , x /= a || y /= b -- rule out the origin
                                ]

data Located a = Located { locPos   :: Position
                         , locValue :: a
                         } deriving (Show)


-- Viewport --------------------------------------------------------------------

data Rect = Rect { rectX, rectY, rectW, rectH :: !Int
                 } deriving (Show)

rectPositions :: Rect -> [[Position]]
rectPositions v = [ row x | x <- [ rectX v .. rectX v + rectW v ] ]
  where
  ys    = [ rectY v .. rectY v + rectH v ]
  row x = [ (x,y) | y <- ys ]
