{-# LANGUAGE MultiParamTypeClasses, DataKinds, KindSignatures, TypeOperators, FlexibleInstances #-}

module Math.Vec where

import GHC.TypeLits
import qualified Data.Vector as V

data Vec (n::Nat) a = Vec (V.Vector a)

-- constructors

  -- unsafe, but available for convenience
vector :: [a] -> Vec n a
vector xs = Vec $ V.fromList xs

  -- the nil vector
nil :: Vec 0 a
nil = Vec V.empty

  -- standard vector construction semantics
cons :: a -> Vec n a -> Vec (n+1) a
cons s (Vec ss) = Vec (V.cons s ss)

infixr 5 &
(&) :: a -> Vec n a -> Vec (n+1) a
(&) = cons

snoc :: Vec n a -> a -> Vec (n+1) a
snoc (Vec ss) s = Vec (V.snoc ss s)

infixr 5 +++
(+++) :: Vec n a -> Vec m a -> Vec (n+m) a
(+++) (Vec xs) (Vec ys) = Vec (xs V.++ ys)

-- indexing
infixr 5 !
(!) :: Vec n a -> Int -> a
(!) (Vec xs) index = xs V.! index

infixr 5 !?
(!?) :: Vec n a -> Int -> Maybe a
(!?) (Vec xs) index = xs V.!? index

dim :: Vec n a -> Int
dim (Vec xs) = V.length xs

-- Haskell idioms
vmap :: (a -> b) -> Vec n a -> Vec n b
vmap f (Vec xs) = Vec $ V.map f xs

vzip :: Vec n a -> Vec n b -> Vec n (a,b)
vzip (Vec xs) (Vec ys) =  Vec $ V.zip xs ys

vsum :: Num a => Vec n a -> a
vsum (Vec xs) = V.sum xs

-- commonly used vector types

type Vec2 = Vec 2 Double

vec2 :: Double -> Double -> Vec2
vec2 x1 x2 = x1 & x2 & nil

type Vec3 = Vec 3 Double

vec3 :: Double -> Double -> Double -> Vec3
vec3 x1 x2 x3 = x1 & x2 & x3 & nil

type Vec4 = Vec 4 Double

vec4 :: Double -> Double -> Double -> Double -> Vec4
vec4 x1 x2 x3 x4 = x1 & x2 & x3 & x4 & nil

  -- in case we neex4 float versions of vectors
type Vec2f = Vec 2 Float

vec2f :: Float -> Float -> Vec2f
vec2f x1 x2 = x1 & x2 & nil

type Vec3f = Vec 3 Float

vec3f :: Float -> Float -> Float -> Vec3f
vec3f x1 x2 x3 = x1 & x2 & x3 & nil

type Vec4f = Vec 4 Float

vec4f :: Float -> Float -> Float -> Float -> Vec4f
vec4f x1 x2 x3 x4 = x1 & x2 & x3 & x4 & nil

-- commonly used vector accessors

class VecAccessors (n::Nat) where
  x :: Vec n a -> a
  y :: Vec n a -> a
  z :: Vec n a -> a
  w :: Vec n a -> a
  xy :: Vec n a -> Vec 2 a
  xyz :: Vec n a -> Vec 3 a
  r :: Vec n a -> a
  g :: Vec n a -> a
  b :: Vec n a -> a
  a :: Vec n a -> a

instance VecAccessors 2 where
  x v = v ! 0
  y v = v ! 1
  z = error "z undefined on Vec2s!"
  w = error "w undefined on Vec2s!"
  xy v = v
  xyz = error "xyz undefined on Vec2s!"
  r = x
  g = y
  b = error "blue undefined on Vec2s!"
  a = error "alpha undefined on Vec2s!"

instance VecAccessors 3 where
  x v = v ! 0
  y v = v ! 1
  z v = v ! 2
  w = error "w undefined on Vec3s!"
  xy (Vec v) = Vec (V.take 2 v)
  xyz v = v
  r = x
  g = y
  b = z
  a = error "alpha undefined on Vec3s!"

instance VecAccessors 4 where
  x v = v ! 0
  y v = v ! 1
  z v = v ! 2
  w v = v ! 3
  xy (Vec v) = Vec (V.take 2 v)
  xyz (Vec v) = Vec (V.take 3 v)
  r = x
  g = y
  b = z
  a = w

-- define componentwise operations for supported numeric Vecs (just doubles and floats for now)
instance (Floating a) => Num (Vec n a)
  where
    v1 + v2                               = vmap (uncurry (+)) $ vzip v1 v2
    v1 - v2                               = vmap (uncurry (-)) $ vzip v1 v2
    v1 * v2                               = vmap (uncurry (*)) $ vzip v1 v2
    negate                                = vmap (\v' -> -v')
    abs                                   = norm
    signum                                = undefined
    fromInteger                           = undefined


-- I really wish we had some notion of operator overloading in Haskell so * can be defined for scalar-vector products
class VectorScalarOps a where
  (***) :: (Num a) => a -> Vec n a -> Vec n a
  (///) :: (Num a) => Vec n a -> a -> Vec n a

instance VectorScalarOps Float where
  s *** v = vmap (*s) v
  v /// s = vmap (/s) v

instance VectorScalarOps Double where
  s *** v = vmap (*s) v
  v /// s = vmap (/s) v

-- other useful operations
dot :: Num a => Vec n a -> Vec n a -> a
dot v1 v2 = vsum $ vmap (uncurry (*)) $ vzip v1 v2

lensq :: Num a => Vec n a -> a
lensq v = vsum $ vmap (\v' -> v' * v') v

len :: Floating a => Vec n a -> a
len v = sqrt $ lensq v

norm :: Floating a => Vec n a -> Vec n a
norm xs = vmap (/l) xs where
  l = len xs

-- cross product is only well-defined for 3 and 7-dimensional vectors
class CrossProduct (n::Nat) where
  cross :: Num a => Vec n a -> Vec n a -> Vec n a

instance CrossProduct 3 where
  cross u v = vector [uy*vz - uz*vy, uz*vx - ux*vz, ux*vy - uy*vx] where
    ux = x u
    uy = y u
    uz = z u
    vx = x v
    vy = y v
    vz = z v

instance CrossProduct 7 where
  cross v1 v2 = vector [s1, s2, s3, s4, s5, s6, s7] where
    s1 = x2*y4 - x4*y2 + x3*y7 - x7*y3 + x5*y6 - x6*y5
    s2 = x3*y5 - x5*y3 + x4*y1 - x1*y4 + x6*y7 - x7*y6
    s3 = x4*y6 - x6*y4 + x5*y2 - x2*y5 + x7*y1 - x1*y7
    s4 = x5*y7 - x7*y5 + x6*y3 - x3*y6 + x1*y2 - x2*y1
    s5 = x6*y1 - x1*y6 + x7*y4 - x4*y7 + x2*y3 - x3*y2
    s6 = x7*y2 - x2*y7 + x1*y5 - x5*y1 + x3*y4 - x4*y3
    s7 = x1*y3 - x3*y1 + x2*y6 - x6*y2 + x4*y5 - x5*y4
    x1 = v1 ! 0
    x2 = v1 ! 1
    x3 = v1 ! 2
    x4 = v1 ! 3
    x5 = v1 ! 4
    x6 = v1 ! 5
    x7 = v1 ! 6
    y1 = v2 ! 0
    y2 = v2 ! 1
    y3 = v2 ! 2
    y4 = v2 ! 3
    y5 = v2 ! 4
    y6 = v2 ! 5
    y7 = v2 ! 6

-- debugging helpers
instance Show a => Show (Vec n a) where
  show v = "(" ++ concatMap formatValueAtIndex [0..(dim v - 1)] ++ ")" where
    formatValueAtIndex i
      | i == dim v - 1 = show $ v ! i
      | otherwise      = show (v ! i) ++ ", "
