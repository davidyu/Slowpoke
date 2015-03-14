{-# LANGUAGE MultiParamTypeClasses, DataKinds, KindSignatures, TypeOperators, FlexibleInstances, OverlappingInstances, TypeSynonymInstances, IncoherentInstances #-}

module Math.Matrix where

import Math.Vec hiding (r,g,b,a,x,y,z,w)
import qualified Math.Vec as V (r,g,b,a,x,y,z,w)
import GHC.TypeLits

type Matrix n m a = Vec n (Vec m a)

-- composition

consx :: Vec n a -> Matrix n m a -> Matrix n (m+1) a
consx v m = vmap (uncurry $ cons) $ vzip v m

snocx :: Matrix n m a -> Vec n a -> Matrix n (m+1) a
snocx m v = vmap (uncurry $ snoc) $ vzip m v

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

instance Num a => ScalarOps (Matrix n m a) a where
  s *** m = vmap (s ***) m

instance Fractional a => ScalarFracOps (Matrix n m a) a where
  m /// s = vmap (/// s) m

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

tensor :: Num a => Vec n a -> Matrix n n a
tensor v = vector $ map row' [0..(dim v - 1)] where
  row' i = vmap (* (v ! i)) v

-- typesafe id constructors
identity4 :: Num a => Matrix 4 4 a
identity4 = identity 4

identity3 :: Num a => Matrix 3 3 a
identity3 = identity 3

transpose :: Matrix n m a -> Matrix m n a
transpose m = vector $ map (m <|>) [0..(dimx m - 1)]

-- Doolittle LU decomposition
lu :: Fractional a => Matrix n n a -> (Matrix n n a, Matrix n n a) -- first is L, second is U
lu m = if dim m == dimy m then doolittle (identity $ dim m) m 0 else error "matrix not square" where
  mdim = dim m
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
det m
  | dim m == 1 = m |-> (0,0)
  | dim m == 2 = let a = m |-> (0,0)
                     b = m |-> (0,1)
                     c = m |-> (1,0)
                     d = m |-> (1,1)
                  in a*d - b*c
  | dim m == 3 = let a = m |-> (0,0)
                     b = m |-> (0,1)
                     c = m |-> (0,2)
                     d = m |-> (1,0)
                     e = m |-> (1,1)
                     f = m |-> (1,2)
                     g = m |-> (2,0)
                     h = m |-> (2,1)
                     i = m |-> (2,2)
                 in a*e*i + b*f*g + c*d*h - c*e*g - b*d*i - a*f*h
  | otherwise = tridet u where
    (_,u) = lu m
    tridet mat = foldl (*) 1 $ map (\i -> mat |-> (i,i)) [0..(dim mat - 1)]

-- solve Ax = B for x using forward substitution, where A is an nxn matrix, B is a 1xn vector, and return value x is a 1xn vector
-- assumes A is a lower triangular matrix
forwardsub :: Fractional a => Matrix n n a -> Vec n a -> Vec n a
forwardsub a b = fs a b 0 where
  fs m v n
    | n == dim v = v
    | otherwise  = fs m v' (n+1) where
      v' = v // [(n, ((v ! n) - fsum) / m_nn)] where
        fsum = sum $ map (\i -> (m |-> (n,i)) * (v ! i)) [0..(n - 1)] -- m_n0 * v_0 + m_n1 * v_1 ... + m_nn-1 * v_n-1
        m_nn = m |-> (n,n)

-- solve Ax = B for x using backward substitution, where A is an nxn matrix, B is a 1xn vector, and return value x is a 1xn vector
-- assumes A is an upper triangular matrix
backsub :: Fractional a => Matrix n n a -> Vec n a -> Vec n a
backsub a b = bs a b (dim b - 1) where
  bs m v n
    | n == -1 = v
    | otherwise = bs m v' (n-1) where
      v' = v // [(n, ((v ! n) - fsum) / m_nn)] where
        fsum = sum $ map (\i -> (m |-> (n,i)) * (v ! i)) [(n + 1)..(dim v - 1)]
        m_nn = m |-> (n,n)

-- returns the trace of a matrix
tr :: Num a => Matrix n n a -> a
tr m
  | dimx m /= dimy m = error "matrix not square"
  | otherwise = sum $ map (\i -> m |-> (i,i)) [0..(dim m - 1)]

inv :: Fractional a => Matrix n n a -> Matrix n n a
inv m
  | dimx m /= dimy m = error "matrix not square"
  | dim m == 2 = let d = det m
                     t  = tr m
                 in (1/d) *** (t *** identity 2 - m)
  | dim m == 3 = let d = det m
                     t = tr m
                     t' = tr (matmul m m)
                     mm = matmul m m
                 in (1/d) *** ((0.5 * (t*t - t')) *** identity 3 - t *** m + mm)
  | dim m == 4 = let d = det m
                     t = tr m
                     m2 = matmul m m
                     m3 = matmul m m2
                     t2 = tr m2
                     t3 = tr m3
                     a = (1/6) * ((t*t*t) - (3*t*t2) + (2*t3))
                     b = (1/2) * ((t*t) - t2)
                 in (1/d) *** (a *** identity 4 - b *** m + t *** m2 - m3)
  | otherwise = transpose $ vector $ map col' [0..(dim m - 1)] where
    col' i = backsub u $ forwardsub l (idm <|> i) where
      (l,u) = lu m
    idm = identity $ dim m

-- graphics utilities

xaxis = vec3 1 0 0
yaxis = vec3 0 1 0
zaxis = vec3 0 0 1

rotationmat :: Vec3 -> Double -> Mat4
rotationmat axis angle = make44 $ cos r *** identity3 + sin r *** crossmat + (1 - cos r) *** tensor axis
  where r = angle * pi / 180
        crossmat = mat3   0  (-z)   y
                          z    0  (-x)
                        (-y)   x    0
                  where
                    x = V.x axis
                    y = V.y axis
                    z = V.z axis

        make44 m = snocx (snoc m (vec3 0 0 0)) (vec4 0 0 0 1)


scalemat :: Double -> Double -> Double -> Mat4
scalemat x y z = mat4 x 0 0 0
                      0 y 0 0
                      0 0 z 0
                      0 0 0 1

translationmat :: Vec3 -> Mat4
translationmat v = mat4 1 0 0 (V.x v)
                        0 1 0 (V.y v)
                        0 0 1 (V.z v)
                        0 0 0 1

-- debugging helpers
instance (Show a) => Show (Matrix n m a) where
  show m = concatMap showRow [0..(dimy m - 1)] where
    showRow r = concatMap formatRowValueAtColumn [0..(dimx m - 1)] where
      formatRowValueAtColumn c
        | c == dimx m - 1 && r == dimy m - 1 = show (m |-> (r,c))
        | c == dimx m - 1                    = show (m |-> (r,c)) ++ "\n"
        | otherwise                          = show (m |-> (r,c)) ++ "\t"
