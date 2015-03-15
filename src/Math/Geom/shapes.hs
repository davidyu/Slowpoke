module Math.Geom.Shapes where

import Math.Vec
import Math.Geom.Primitives

data Shape = Sphere Point Double
           | Triangle (Point, Point, Point)
             deriving (Show)
