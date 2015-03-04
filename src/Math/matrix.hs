{-# LANGUAGE MultiParamTypeClasses, DataKinds, KindSignatures, TypeOperators, FlexibleInstances, OverlappingInstances, TypeSynonymInstances, IncoherentInstances #-}

module Math.Matrix where

import Math.Vec hiding (r,g,b,a,x,y,z,w)
import GHC.TypeLits

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
col mat c = vmap (!c) mat

infixr 5 <|>
(<|>) :: Matrix m n a -> Int -> Vec m a
(<|>) = col

dimy :: Matrix m n a -> Int
dimy = dim

dimx :: Matrix m n a -> Int
dimx mat = dim $ mat <-> 0

-- commonly used matrix types

type Mat3 = Matrix 3 3 Double

mat3 :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Mat3
mat3 a b c d e f g h i
  = (a & b & c & nil)
  & (d & e & f & nil)
  & (g & h & i & nil)
  & nil

type Mat4 = Matrix 4 4 Double

mat4 :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Mat4
mat4 a b c d e f g h i j k l m n o p
  = (a & b & c & d & nil)
  & (e & f & g & h & nil)
  & (i & j & k & l & nil)
  & (m & n & o & p & nil)
  & nil

type Mat3f = Matrix 3 3 Float

mat3f :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Mat3f
mat3f a b c d e f g h i
  = (a & b & c & nil)
  & (d & e & f & nil)
  & (g & h & i & nil)
  & nil

type Mat4f = Matrix 4 4 Float

mat4f :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Mat4f
mat4f a b c d e f g h i j k l m n o p
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
  tz     = error "tz undefined for Mat33!"
  sx mat = mat |-> (0,0)
  sy mat = mat |-> (1,1)
  sz     = error "scale z undefined for Mat33!"

instance HomogeneousMatAccessors 4 where
  tx mat = mat |-> (0,3)
  ty mat = mat |-> (1,3)
  tz mat = mat |-> (2,3)
  sx mat = mat |-> (0,0)
  sy mat = mat |-> (1,1)
  sz mat = mat |-> (2,2)

-- common operations
matmul :: Num a => Matrix n m a -> Matrix m p a -> Matrix n p a
matmul a b = vector $ map rowc [0..(dimy a - 1)] where
  rowc r = vector $ map (\c -> dot ar (b <|> c)) [0..(dimx b - 1)] where
    ar = a <-> r

vecmat :: Num a => Vec n a -> Matrix n m a -> Vec m a
vecmat v m = vector $ map (\c -> dot v (m <|> c)) [0..(dimx m - 1)]

matvec :: Num a => Matrix n m a -> Vec m a -> Vec n a
matvec m v = vector $ map (\r -> dot (m <-> r) v) [0..(dimy m - 1)]

identity :: Num a => Int -> Matrix n n a
identity n = vector $ map row' [0..(n-1)] where
  row' i = vector $ replicate i 0 ++ 1:replicate (n-i-1) 0

-- typesafe id constructors
identity4 :: Num a => Matrix 4 4 a
identity4 = identity 4

identity3 :: Num a => Matrix 3 3 a
identity3 = identity 3

transpose :: Matrix n m a -> Matrix m n a
transpose m = vector $ map (m <|>) [0..(dimx m - 1)]

-- Doolittle LU decomposition
lu :: Fractional a => Matrix n n a -> (Matrix n n a, Matrix n n a) -- first is L, second is U
lu m = if dimx m == dimy m then doolittle (identity $ dimx m) m 0 else error "matrix not square" where
  mdim = dimx m
  doolittle l a n
    | n == mdim-1 = (l,a)
    | otherwise  = doolittle l' a' (n+1) where
                     l' = matmul l ln' -- ln' is inverse of ln
                     a' = matmul ln a
                     ln = vector (map id' [0..n] ++ map lr' [(n+1)..(mdim-1)]) where
                       lr' i = vector $ replicate n 0 ++ [-(ai' i)] ++ replicate (i-n-1) 0 ++ 1:replicate (mdim-i-1) 0
                     ln' = vector (map id' [0..n] ++ map lr' [(n+1)..(mdim-1)]) where
                       lr' i = vector $ replicate n 0 ++ [ai' i] ++ replicate (i-n-1) 0 ++ 1:replicate (mdim-i-1) 0
                     id' i = vector $ replicate i 0 ++ 1:replicate (mdim-i-1) 0
                     ai' i = (a |-> (i,n)) / (a |-> (n,n))

det :: Fractional a => Matrix n n a -> a
det m = tridet u where
  (_,u) = lu m
  tridet mat = foldl (*) 1 $ map (\i -> mat |-> (i,i)) [0..(dimx mat - 1)]

-- debugging helpers
instance (Show a) => Show (Matrix n m a) where
  show m = concatMap showRow [0..(dimy m - 1)] where
    showRow r = concatMap formatRowValueAtColumn [0..(dimx m - 1)] where
      formatRowValueAtColumn c
        | c == dimx m - 1 && r == dimy m - 1 = show (m |-> (r,c))
        | c == dimx m - 1                    = show (m |-> (r,c)) ++ "\n"
        | otherwise                          = show (m |-> (r,c)) ++ "\t"
