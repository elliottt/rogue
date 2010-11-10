module Math.Utils (
    module Math.Utils
  , GLfloat
  ) where

import Graphics.Rendering.OpenGL.GL (GLfloat)

rangeOverlap :: (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> GLfloat
rangeOverlap (a1,b1) (a2,b2)
  | a2 > a1   = b1 - a2
  | otherwise = b2 - a1

class HasZero a where
  zero   :: a
  isZero :: a -> Bool

instance HasZero GLfloat where
  zero     = 0
  isZero 0 = True
  isZero _ = False

withinZero :: GLfloat -> Bool
withinZero z = abs z <= 0.0001

