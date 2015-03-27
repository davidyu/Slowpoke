import Codec.Picture
import Codec.Picture.Types
import Data.Char
import Debug.Trace
import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Raster.Field
import System.Environment

import Math.Geom.Primitives hiding (Point)
import Math.Geom.Shapes
import Math.Geom.Intersections
import Parser
import qualified Math.Vec as V
import Math.Vec (( *** ), ( /// ))
import Math.Matrix

type Sample = (Double, Double)

data Viewport = Viewport { upperleft  :: V.Vec3
                         , upperright :: V.Vec3
                         , lowerleft  :: V.Vec3
                         , lowerright :: V.Vec3
                         } deriving (Show)

vp :: Params -> Viewport
vp p = Viewport { upperleft  = (center + (-halfw) *** right + halfh *** up')
                , upperright = (center + halfw *** right + halfh *** up')
                , lowerright = (center + halfw *** right + (-halfh) *** up')
                , lowerleft  = (center + (-halfw) *** right + (-halfh) *** up') }
         where eye' = eye $ cam p
               center = target $ cam p
               up' = up $ cam p
               eyedir = V.norm (center - eye')
               right = V.norm (eyedir `V.cross` up')
               Size outputw outputh = sze p
               fovy = fov $ cam p
               fovx = (fromIntegral outputw) * fovy / (fromIntegral outputh)
               halfh = V.len (center - eye') * (tan $ toRad (fovy/2))
               halfw = V.len (center - eye') * (tan $ toRad (fovx/2))
               toRad deg = deg * pi / 180

main
  = do args <- getArgs
       case args of [ input ] -> do params <- paramsFromFile input
                                    Size screenw screenh <- return $ sze params
                                    filename <- return $ out params
                                    case filename of "display" -> display ( InWindow input (screenw, screenh) (0, 0) ) black $ (gather params screenw screenh)
                                                                    where gather params w h = let viewport = vp params
                                                                                                  eye' = eye $ cam params
                                                                                              in makePicture w h 1 1 (\p -> raytrace eye' (ray eye' viewport (glossPointToSample p)) (objs params) (rig params) )
                                                     otherwise -> writePng filename $ generateImage (gather params screenw screenh) screenw screenh
                                                                    where gather params w h x y = let viewport = vp params
                                                                                                      eye' = eye $ cam params
                                                                                                  in colorToPixel $ raytrace eye' (ray eye' viewport ( 1 - (fromIntegral x) / (fromIntegral w), (fromIntegral y) / (fromIntegral h) ) ) (objs params) (rig params)
                    otherwise -> print "usage: ./main input.txt"

-- interpolates from (-1, 1) to (0, 1)
glossPointToSample :: Point -> Sample
glossPointToSample (x, y) = (1 - (float2Double x + 1)/2, 1 - (float2Double y + 1)/2)

-- translation from gloss color to codec picture pixel
colorToPixel :: Color -> PixelRGB8
colorToPixel c = let ( r, g, b, _ ) = rgbaOfColor c
                 in PixelRGB8 (round (r * 255)) (round (g * 255)) (round (b * 255))

ray :: V.Vec3 -> Viewport -> Sample -> Ray
ray eye viewport (u, v) =
  let ul = upperleft  viewport
      ur = upperright viewport
      ll = lowerleft  viewport
      lr = lowerright viewport
      pt = u *** ( v *** ll + ( 1 - v ) *** ul ) + ( 1 - u ) *** ( v *** lr + ( 1 - v ) *** ur )
  in Ray eye $ V.norm (pt - eye)

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
