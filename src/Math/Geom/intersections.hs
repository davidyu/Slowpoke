module Math.Geom.Intersections where

import Math.Vec
import Math.Matrix
import Math.Geom.Primitives
import Math.Geom.Shapes

epsilon = 0.000001

-- applies transform to ray
apply :: Mat4 -> Ray -> Ray
apply transform (Ray origin dir) =
  Ray origin' dir' where
    origin' = origin + xyz (transform <|> 3)
    dir'    = xyz $ vecmat (snoc dir 0) transform

data XsectResult = Miss | Hit [(Double, Point, Vec3)] -- Hit [(t, intersection point, intersection normal)]

-- utility; ray plane intersection
-- ideally this is folded into intersect
rayplane :: Ray -> Plane -> XsectResult
rayplane ray (Plane point normal) = 
  if denom == 0 then Miss -- line is parallel to the plane, treat as if miss
  else Hit [(t, p, normal)]
    where Ray origin direction = ray
          denom = dot normal direction
          d = dot (-normal) point
          t = -((dot normal origin) + d) / denom
          p = ray |@| t

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
                    n1 = norm (p1 - ct)
                    n2 = norm (p2 - ct)
                in (Hit [ (t1, p1, n1), (t2, p2, n2)])
              | otherwise =
                let t = (-b) / (2 * a)
                    p = s + t *** v
                    n = norm (p - ct)
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
  in if isBarycentric u v && t > 0 then Hit [(t, origin + t *** direction, norm n)] else Miss
    where isBarycentric u v = u >= 0.0 && v >= 0.0 && (u + v) - 1.0 <= epsilon
intersect ray (Box dim, _) =
  let d = dot (-n) planept
      t = -(dot n origin + d) / (dot n direction)
      ipt = ray |@| t
  in if x ipt >= -x dim && x ipt <= x dim && y ipt >= -y dim && y ipt <= y dim && z ipt >= -z dim && z ipt <= z dim
     then Hit [(t, ipt, n)]
     else Miss
      where Ray origin direction = ray
            Plane n planept = [ Plane (vec3 (x dim) 0 0)  (vec3 (x dim) 0 0)
                              , Plane (vec3 (-x dim) 0 0) (vec3 (-x dim) 0 0)
                              , Plane (vec3 0 (y dim) 0)  (vec3 0 (y dim) 0)
                              , Plane (vec3 0 (-y dim) 0) (vec3 0 (-y dim) 0)
                              , Plane (vec3 0 0 (z dim))  (vec3 0 0 (z dim))
                              , Plane (vec3 0 0 (-z dim)) (vec3 0 0 (-z dim))
                              ] `closestTo` origin
            closestTo :: [Plane] -> Point -> Plane
            closestTo (p:ps) pt = closestTo' ps pt (dist pt p, p) where
                                   closestTo' (plane:ps) pt (closestDist, closestPlane) =
                                     if dist pt plane < closestDist then closestTo' ps pt (dist pt plane, plane)
                                                                    else closestTo' ps pt (closestDist, closestPlane)
                                   closestTo' [] _ (_, closestPlane) = closestPlane
