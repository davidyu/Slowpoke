{-# LANGUAGE QuasiQuotes #-}

module Math.Geom.Intersections where

import Math.Vec
import Math.Matrix
import Math.Geom.Primitives
import Math.Geom.Shapes
import Record
import Record.Lens

epsilon = 0.000001

-- applies transform to ray
apply :: Mat4 -> Ray -> Ray
apply transform (Ray origin dir) =
  Ray origin' dir' where
    origin' = origin + xyz (transform <|> 4)
    dir'    = xyz $ vecmat (snoc dir 0) transform

data XsectResult = Miss | Hit [(Double, Point, Vec3)] -- Hit [(t, intersection point, intersection normal)]

intersect :: Ray -> (Shape, Mat4) -> XsectResult
intersect ray (Sphere center radius, xf) =
  let  Ray origin direction = apply (inv xf) ray
       a = lensq direction
       b = 2 * dot direction (origin - center)
       c = lensq (origin - center) - radius^2
  in getRaySphereXSection a b c (b * b - 4 * a * c) center origin direction
     where getRaySphereXSection a b c discr ct s v
              | discr < (-epsilon) = Miss
              | discr > epsilon  =
                let t1 = ((-b) - sqrt discr) / (2 * a)
                    t2 = ((-b) + sqrt discr) / (2 * a)
                    p1 = s + t1 *** v
                    p2 = s + t2 *** v
                    n1 = abs (p1 - ct)
                    n2 = abs (p2 - ct)
                in (Hit [ (t1, p1, n1), (t2, p2, n2)])
              | otherwise =
                let t = (-b) / (2 * a)
                    p = s + t *** v
                    n = abs (p - ct)
                in (Hit [(t, p, n)])
intersect ray (Triangle (v1, v2, v3), xf) =
  let Ray origin direction = apply (inv xf) ray
      e1 = v2 - v1
      e2 = v3 - v1
      n = e1 `cross` e2
      s = origin - v1
      m = s `cross` direction
      result = (1/(-(n `dot` direction))) *** vec3 (n `dot` s) (m `dot` e2) ((-m) `dot` e1)
      t = x result
      u = y result
      v = z result
  in if isBarycentric u v && t > 0 then Hit [(t, origin + t *** direction, abs n)] else Miss
    where isBarycentric u v = u >= 0.0 && v >= 0.0 && (u + v) - 1.0 <= epsilon
