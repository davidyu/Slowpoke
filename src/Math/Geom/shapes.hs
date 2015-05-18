module Math.Geom.Shapes where

import Math.Vec
import Math.Geom.Primitives

data Shape = Sphere Point Double -- center radius
           | Triangle (Point, Point, Point)
           | Box Vec3 -- center dimensions (w,h,l)
             deriving (Show)
