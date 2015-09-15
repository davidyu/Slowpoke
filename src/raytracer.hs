module Raytracer where

import GHC.Float
import Graphics.Gloss

import Math.Geom.Primitives hiding (Point)
import Math.Geom.Shapes
import Math.Geom.Intersections
import Parser -- get rid of this dependency!
import qualified Math.Vec as V
import Math.Vec (( *** ), ( /// ))
import Math.Matrix

-- TODO: make accumulate readable
-- sort intersect results by t, no conditional in accumulate
raytrace :: V.Vec3 -> Ray -> [(Shape, Material, Mat4)] -> Rig -> Color
raytrace eye ray [] rig   = ka rig
raytrace eye ray objs rig = accumulate (ka rig, 1/0) rig (map (\(shape, mat, xf) -> (intersect ray (shape, xf), mat)) objs)
  where accumulate :: (Color, Double) -> Rig -> [(XsectResult, Material)] -> Color
        accumulate (c, t) rig ((Miss, _):xs)      = accumulate (c, t) rig xs
        accumulate (c, t) rig ((Hit res, mat):xs)  = let (t', pt, n) = head res --ignore rest of res
                                                         acc = if t' < t then (computeLight (ka rig + ke mat) (lights rig) (eye) pt n mat, t') else (c, t)
                                                     in accumulate acc rig xs
        accumulate (c, _) rig []                   = c

        computeLight :: Color -> [Light] -> V.Vec3 -> V.Vec3 -> V.Vec3 -> Material -> Color
        computeLight acc (light:ls) eye pos normal mat
          = let col = case light of PointLight _ lightcol -> lightcol
                                    DirectionalLight _ lightcol -> lightcol
                l = case light of PointLight lightpos _ -> V.norm (lightpos - pos)
                                  DirectionalLight lightdir _ -> V.norm lightdir
                h = V.norm (V.norm (eye - pos) + l)
                diffuse = (kd mat) * (greyN $ double2Float (max (normal `V.dot` l) 0))
                specular = (ks mat) * (greyN $ double2Float ((max (normal `V.dot` h) 0) ** (sh mat)))
                acc' = acc + col * (diffuse + specular)
            in computeLight acc' ls eye pos normal mat
        computeLight acc [] eye pos normal mat = acc
