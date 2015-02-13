{-# LANGUAGE MultiParamTypeClasses, DataKinds, KindSignatures, TypeOperators, ExistentialQuantification #-}

module Math.Vec where

import GHC.TypeLits

data Vec (n::Nat) a = Vec [a] deriving (Show)

-- unsafe, but available for convenience
vector :: forall n a. (KnownNat n, Num a) => [a] -> Vec n a
vector xs = Vec xs

nil :: forall a. Num a => Vec 0 a
nil = Vec []

cons :: forall a n. (KnownNat n, Num a) => a -> Vec n a -> Vec (n+1) a
cons x (Vec xs) = Vec (x:xs)

snoc :: forall a n. (KnownNat n, Num a) => Vec n a -> a -> Vec (n+1) a
snoc (Vec xs) x = Vec (xs ++ [x])

concatv :: forall a n m. (KnownNat n, KnownNat m, Num a) => Vec n a -> Vec m a -> Vec (n+m) a
concatv (Vec xs) (Vec ys) = Vec (xs ++ ys)

type Vec2f = Vec 2 Float
type Vec3f = Vec 3 Float
type Vec4f = Vec 4 Float

type Vec2 = Vec 2 Double
type Vec3 = Vec 3 Double
type Vec4 = Vec 4 Double
