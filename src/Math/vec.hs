{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, ExistentialQuantification, FlexibleInstances #-}

module Math.Vec where

import GHC.TypeLits
import qualified Data.Vector as V

data Vec (n::Nat) a = Vec (V.Vector a) deriving (Show)

-- constructors

  -- unsafe, but available for convenience
vector :: forall n a. (KnownNat n) => [a] -> Vec n a
vector xs = Vec $ V.fromList xs

  -- the nil vector
nil :: Vec 0 a
nil = Vec V.empty

  -- standard vector construction semantics
cons :: forall a n. (KnownNat n) => a -> Vec n a -> Vec (n+1) a
cons x (Vec xs) = Vec (V.cons x xs)

infixr 5 &
(&) :: forall a n. (KnownNat n) => a -> Vec n a -> Vec (n+1) a
(&) = cons

snoc :: forall a n. (KnownNat n) => Vec n a -> a -> Vec (n+1) a
snoc (Vec xs) x = Vec (V.snoc xs x)

infixr 5 ++
(++) :: forall a n m. (KnownNat n, KnownNat m) => Vec n a -> Vec m a -> Vec (n+m) a
(++) (Vec xs) (Vec ys) = Vec (xs V.++ ys)

-- indexing

infixr 5 !
(!) :: forall n a. (KnownNat n) => Vec n a -> Int -> a
(!) (Vec xs) index = xs V.! index

infixr 5 !?
(!?) :: forall n a. (KnownNat n) => Vec n a -> Int -> Maybe a
(!?) (Vec xs) index = xs V.!? index

-- commonly used vector types

type Vec2 = Vec 2 Double

vec2 :: Double -> Double -> Vec2
vec2 a b = a & b & nil

type Vec3 = Vec 3 Double

vec3 :: Double -> Double -> Double -> Vec3
vec3 a b c = a & b & c & nil

type Vec4 = Vec 4 Double

vec4 :: Double -> Double -> Double -> Double -> Vec4
vec4 a b c d = a & b & c & d & nil

  -- in case we need float versions of vectors
type Vec2f = Vec 2 Float

vec2f :: Float -> Float -> Vec2f
vec2f a b = a & b & nil

type Vec3f = Vec 3 Float

vec3f :: Float -> Float -> Float -> Vec3f
vec3f a b c = a & b & c & nil

type Vec4f = Vec 4 Float

vec4f :: Float -> Float -> Float -> Float -> Vec4f
vec4f a b c d = a & b & c & d & nil

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
