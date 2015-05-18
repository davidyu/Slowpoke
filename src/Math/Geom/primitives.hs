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
