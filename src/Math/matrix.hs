{-# LANGUAGE MultiParamTypeClasses, DataKinds, KindSignatures, TypeOperators, ExistentialQuantification, FlexibleInstances #-}

module Math.Matrix where

import Math.Vec
import GHC.TypeLits

type Matrix m n a = Vec m (Vec n a)

-- indexing
  -- shorthand
infixr 5 |->
(|->) :: forall a m n. (KnownNat m, KnownNat n) => Matrix m n a -> (Int, Int) -> a
(|->) m (r,c) = (m ! r) ! c

  -- row vector
row :: forall a m n. (KnownNat m, KnownNat n) => Matrix m n a -> Int -> Vec n a
row mat r = mat ! r

  -- col vector
col :: forall a m n. (KnownNat m, KnownNat n) => Matrix m n a -> Int -> Vec m a
col mat c = vmap (\v -> v ! c) mat

-- commonly used matrix types

type Mat33 = Matrix 3 3 Double
type Mat44 = Matrix 3 3 Double

class HomogeneousMatAccessors (n::Nat) where
  tx :: Matrix n n a -> a
  ty :: Matrix n n a -> a
  tz :: Matrix n n a -> a
  sx :: Matrix n n a -> a
  sy :: Matrix n n a -> a
  sz :: Matrix n n a -> a

instance HomogeneousMatAccessors 3 where
  tx mat = mat |-> (0,2)
  ty mat = mat |-> (1,2)
  tz mat = error "tz undefined for Mat33!"
  sx mat = mat |-> (0,0)
  sy mat = mat |-> (1,1)
  sz mat = error "scale z undefined for Mat33!"

instance HomogeneousMatAccessors 4 where
  tx mat = mat |-> (0,3)
  ty mat = mat |-> (1,3)
  tz mat = mat |-> (2,3)
  sx mat = mat |-> (0,0)
  sy mat = mat |-> (1,1)
  sz mat = mat |-> (2,2)
