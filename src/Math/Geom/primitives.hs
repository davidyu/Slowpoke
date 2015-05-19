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

class Distance m n where
  dist :: m -> n -> Double

instance Distance Point Point where
  dist a b = len (a - b)

instance Distance Point Plane where
  dist pt plane = len (pt - planept) where
                    Plane planept _ = plane

instance Distance Plane Point where
  dist plane pt = len (planept - pt) where
                    Plane planept _ = plane
