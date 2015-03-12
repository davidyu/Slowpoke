module Math.Geom.Primitives where

import Math.Vec

type Point = Vec3
data Ray = Ray Point Vec3
data Plane = Plane Point Vec3
