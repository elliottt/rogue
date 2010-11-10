module Math.AffinePlane where

import Math.Utils


-- Points ----------------------------------------------------------------------

data Point a = Point !a !a
  deriving (Show,Eq,Ord)

instance HasZero a => HasZero (Point a) where
  zero               = Point zero zero
  isZero (Point x y) = isZero x && isZero y

distance :: Floating a => Point a -> Point a -> a
distance a b = norm (a -. b)

-- | The origin.
zeroP :: Num a => Point a
zeroP  = Point 0 0

-- | Point subtraction.
(-.) :: Num a => Point a -> Point a -> Vector a
Point a b -. Point c d = Vector (a-c) (b-d)
infix 6 -.

-- | Point-vector addition.
(.+^) :: Num a => Point a -> Vector a -> Point a
Point a b .+^ Vector c d = Point (a+c) (b+d)
infix 6 .+^

-- | Point-vector subtraction -- adds the negation of the vector.
(.-^) :: Num a => Point a -> Vector a -> Point a
p .-^ v = p .+^ negV v
infix 6 .-^


-- Vectors ---------------------------------------------------------------------

data Vector a = Vector !a !a
  deriving (Show,Eq)

instance HasZero a => HasZero (Vector a) where
  zero                = Vector zero zero
  isZero (Vector x y) = isZero x && isZero y

-- | Calculate the norm of a vector.
norm :: Floating a => Vector a -> a
norm v = sqrt (v <.> v)

-- | Calculate the squared norm of a vector.
norm2 :: Num a => Vector a -> a
norm2 (Vector x y) = x*x + y*y

-- | Negate a vector.
negV :: Num a => Vector a -> Vector a
negV (Vector x y) = Vector (-x) (-y)

normalV :: Num a => Vector a -> Vector a
normalV (Vector x y) = Vector (-y) x

-- | Add two vectors.
(+^) :: Num a => Vector a -> Vector a -> Vector a
Vector a b +^ Vector c d = Vector (a+c) (b+d)
infixl 6 +^

-- | Scale a vector by a scalar.
(*^) :: Num a => a -> Vector a -> Vector a
r *^ Vector x y = Vector (r*x) (r*y)
infix 5 *^

-- | Subtract two vectors
(-^) :: Num a => Vector a -> Vector a -> Vector a
Vector a b -^ Vector c d = Vector (a-c) (b-d)
infixl 6 -^

-- | The inner product of two vectors.
(<.>) :: Num a => Vector a -> Vector a -> a
Vector a b <.> Vector c d = a * c + b * d
infix 5 <.>

-- | Project one vector along another.
projAlong :: Floating a => Vector a -> Vector a -> Vector a
projAlong a b@(Vector x y) = ((a <.> b) / (x*x + y*y)) *^ b

-- | The affine combination
combination :: Num a => a -> Point a -> a -> Point a -> Point a
combination _a1 p a2 q = p .+^ (a2 *^ (q -. p))

-- | Turn a vector into its unit version.
unitV :: Floating a => Vector a -> Vector a
unitV v
  | n == 0    = v
  | otherwise = (1 / norm v) *^ v
  where
  n = norm v


-- Affine Transformations ------------------------------------------------------

data Matrix a = Matrix
  { mat00 :: !a, mat01 :: !a, mat02 :: !a
  , mat10 :: !a, mat11 :: !a, mat12 :: !a
  } deriving (Eq,Show)

instance HasZero a => HasZero (Matrix a) where
  zero = Matrix
    { mat00 = zero, mat01 = zero, mat02 = zero
    , mat10 = zero, mat11 = zero, mat12 = zero
    }
  isZero m = isZero (mat00 m) && isZero (mat01 m) && isZero (mat02 m) &&
             isZero (mat10 m) && isZero (mat11 m) && isZero (mat12 m)

idM :: Num a => Matrix a
idM  = Matrix
  { mat00 = 1, mat01 = 0, mat02 = 0
  , mat10 = 0, mat11 = 1, mat12 = 0
  }

-- | Transform a point by a matrix.
transformPoint :: Num a => Matrix a -> Point a -> Point a
transformPoint m (Point a b) = o .+^ ((a *^ r1) +^ (b *^ r2))
  where
  r1 = Vector (mat00 m) (mat10 m)
  r2 = Vector (mat10 m) (mat11 m)
  o  = Point (mat02 m) (mat12 m)

addMatrix :: Num a => Matrix a -> Matrix a -> Matrix a
addMatrix a b = Matrix
  { mat00 = mat00 a + mat00 b, mat01 = mat01 a + mat01 b
  , mat02 = mat02 a + mat02 b
  , mat10 = mat10 a + mat10 b, mat11 = mat11 a + mat11 b
  , mat12 = mat12 a + mat12 b
  }


-- Lines -----------------------------------------------------------------------

data Line a = Line !(Point a) !(Point a)
  deriving (Eq,Show)

lineV :: Num a => Line a -> Vector a
lineV (Line a b) = a -. b
