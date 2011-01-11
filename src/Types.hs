module Types where

import Data.Function (on)

type Intensity = Int

data Lighted a = Lighted
  { lightedIntensity :: !Intensity
  , lightedData      :: a
  } deriving (Show,Eq)

instance Ord a => Ord (Lighted a) where
  compare = compare `on` lightedData
