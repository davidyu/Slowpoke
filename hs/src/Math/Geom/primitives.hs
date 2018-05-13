{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Math.Geom.Primitives where

import Math.Vec

type Point = Vec3
data Ray = Ray Point Vec3
data Plane = Plane Point Vec3

at :: Ray -> Double -> Point
at (Ray origin direction) t = origin + t *** direction

infixr 5 |@|
(|@|) :: Ray -> Double -> Point
(|@|) = at

class ComparablePrimitivePair m n where
  dist :: m -> n -> Double

instance ComparablePrimitivePair Point Point where
  dist a b = len (a - b)

-- TODO eliminate these extra normalizes; maybe there is a way to mark a vector as normalized?

instance ComparablePrimitivePair Point Plane where
  dist pt plane = dot (pt - planept) $ norm n where
                    Plane planept n = plane

instance ComparablePrimitivePair Plane Point where
  dist plane pt = dot (planept - pt) $ norm n where
                    Plane planept n = plane

instance ComparablePrimitivePair Plane Plane where
  dist a b = if norm n1 == norm n2
             then dot (p1 - p2) $ norm n1
             else 0 where
               Plane p1 n1 = a
               Plane p2 n2 = b
