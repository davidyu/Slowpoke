module Scene where

import Math.Vec
import Math.Matrix
import Math.Geom.Primitives
import Math.Geom.Shapes
import Graphics.Gloss (Color, makeColor)
import qualified Data.Vector as V

data Camera = Camera { eye :: Vec3, target :: Vec3, up :: Vec3, fov :: Double } deriving Show
data Size = Size Int Int deriving Show
-- kd = diffuse, ks = specular, sh = shininess, ke = emission
data Material = Material { kd :: Color, ks :: Color, sh :: Double, ke :: Color } deriving Show
data LightAttenuation = LightAttenuation { constantLightAttenuation :: Double, linearLightAttenuation :: Double, quadraticLightAttenuation :: Double } deriving Show
data Light = DirectionalLight Vec3 Color | PointLight Vec3 Color deriving Show
data Rig = Rig { ka :: Color, att :: LightAttenuation, lights :: [Light] } deriving Show

data Scene = Scene { cam :: Camera
                   , sze :: Size
                   , out :: String
                   , objs :: [(Shape, Material, Mat4)]
                   , rig :: Rig
                   , vxs :: V.Vector Vec3
                   } deriving (Show)
