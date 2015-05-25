module Math.Geom.Intersections where

import Debug.Trace
import Math.Vec
import Math.Matrix
import Math.Geom.Primitives
import Math.Geom.Shapes

epsilon = 0.000001
infinity = 1/0

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
intersect ray (Box dim, xf) =
  let dx = dot xnormal direction
      xt = if dx /= 0 then -(dot xnormal origin + dot (-xnormal) xpt) / dx else (-1)
      dy = dot ynormal direction
      yt = if dy /= 0 then -(dot ynormal origin + dot (-ynormal) ypt) / dy else (-1)
      dz = dot znormal direction
      zt = if dz /= 0 then -(dot znormal origin + dot (-znormal) zpt) / dz else (-1)
      candidates = filter (/= (-1)) [xt, yt, zt]
      t = if length candidates > 0 then findClosestValidT candidates else (-1)
      n = if t == (-1) then (vec3 (-1) (-1) (-1)) else if t == xt then xnormal else if yt == t then ynormal else znormal
      ipt = ray |@| t
  in if t /= -1 then trace "hit" $ Hit [(t, ipt, n)]
                else trace "miss" $ Miss
       where Ray origin direction = ray
             Plane xnormal xpt = [ Plane (vec3 (x dim/2 + tx xf) (ty xf) (tz xf))  (norm $ vec3 (x dim) 0 0)
                                 , Plane (vec3 (-x dim/2 + tx xf) (ty xf) (tz xf)) (norm $ vec3 (-x dim) 0 0)
                                 ] `closestTo` origin
             Plane ynormal ypt = [ Plane (vec3 (tx xf) (y dim/2 + ty xf) (tz xf))  (norm $ vec3 0 (y dim) 0)
                                 , Plane (vec3 (tx xf) (-y dim/2 + ty xf) (tz xf)) (norm $ vec3 0 (-y dim) 0)
                                 ] `closestTo` origin
             Plane znormal zpt = [ Plane (vec3 (tx xf) (ty xf) (z dim/2 + tz xf))  (norm $ vec3 0 0 (z dim))
                                 , Plane (vec3 (tx xf) (ty xf) (-z dim/2 + tz xf)) (norm $ vec3 0 0 (-z dim))
                                 ] `closestTo` origin
             closestTo :: [Plane] -> Point -> Plane
             closestTo (p:ps) pt = closestTo' ps pt (dist pt p, p) where
                                    closestTo' (plane:ps) pt (closestDist, closestPlane) =
                                      if dist pt plane < closestDist then closestTo' ps pt (dist pt plane, plane)
                                                                     else closestTo' ps pt (closestDist, closestPlane)
                                    closestTo' [] _ (_, closestPlane) = closestPlane
             findClosestValidT ts = f' ts infinity where
                                     f' (t:ts) closest = if t < closest
                                                         && x ipt >= (-(x dim)/2 + tx xf) && x ipt <= (x dim/2 + tx xf)
                                                         && y ipt >= (-(y dim)/2 + ty xf) && y ipt <= (y dim/2 + ty xf)
                                                         && z ipt >= (-(z dim)/2 + tz xf) && z ipt <= (z dim/2 + tz xf) then f' ts t else f' ts closest where
                                                           ipt = ray |@| t
                                     f' [] closest     = closest
