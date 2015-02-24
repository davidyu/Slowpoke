{-# LANGUAGE MultiParamTypeClasses, DataKinds, KindSignatures, TypeOperators, ExistentialQuantification, FlexibleInstances, ScopedTypeVariables #-}

module Math.Matrix where

import Math.Vec
import GHC.TypeLits
import Debug.Trace

type Matrix n m a = Vec n (Vec m a)

-- indexing
  -- shorthand
infixr 5 |->
(|->) :: Matrix m n a -> (Int, Int) -> a
(|->) m (r,c) = (m ! r) ! c

  -- row vector
row :: Matrix m n a -> Int -> Vec n a
row mat r = mat ! r

infixr 5 <->
(<->) :: Matrix m n a -> Int -> Vec n a
(<->) = row

  -- col vector
col :: Matrix m n a -> Int -> Vec m a
col mat c = vmap (\v -> v ! c) mat

infixr 5 <|>
(<|>) :: Matrix m n a -> Int -> Vec m a
(<|>) = col

dimy :: Matrix m n a -> Int
dimy mat = dim mat

dimx :: Matrix m n a -> Int
dimx mat = dim $ mat <-> 0

-- commonly used matrix types

type Mat33 = Matrix 3 3 Double

mat33 :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Mat33
mat33 a b c d e f g h i
  = (a & b & c & nil)
  & (d & e & f & nil)
  & (g & h & i & nil)
  & nil

type Mat44 = Matrix 4 4 Double

mat44 :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Mat44
mat44 a b c d e f g h i j k l m n o p
  = (a & b & c & d & nil)
  & (e & f & g & h & nil)
  & (i & j & k & l & nil)
  & (m & n & o & p & nil)
  & nil

type Mat33f = Matrix 3 3 Float

mat33f :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Mat33f
mat33f a b c d e f g h i
  = (a & b & c & nil)
  & (d & e & f & nil)
  & (g & h & i & nil)
  & nil

type Mat44f = Matrix 4 4 Float

mat44f :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Mat44f
mat44f a b c d e f g h i j k l m n o p
  = (a & b & c & d & nil)
  & (e & f & g & h & nil)
  & (i & j & k & l & nil)
  & (m & n & o & p & nil)
  & nil

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

matmul :: Num a => Matrix n m a -> Matrix m p a -> Matrix n p a
matmul a b = vector $ map rowc [0..(dimy a - 1)] where
  rowc r = vector $ map (\c -> dot ar (b <|> c)) [0..(dimx b - 1)] where
    ar = a <-> r

vecmat :: Num a => Vec n a -> Matrix n m a -> Vec m a
vecmat v m = vector $ map (\c -> dot v (m <|> c)) [0..(dimx m - 1)]

matvec :: Num a => Matrix n m a -> Vec m a -> Vec n a
matvec m v = vector $ map (\r -> dot (m <-> r) v) [0..(dimy m - 1)]
