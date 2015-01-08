module Parser where

import Control.Monad
import Control.Applicative hiding ((<|>), many)
import GHC.Float
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token
import Graphics.Gloss (Color, makeColor)

import Math

data Command = CmdSize Int Int
             | CmdMaxdepth Int
             | CmdOutput String
             | CmdCamera Double Double Double Double Double Double Double Double Double Double
             | CmdSphere Double Double Double Double
             | CmdMaxVerts Int
             | CmdVertex Double Double Double
             | CmdTri Int Int Int
             | CmdTranslate Vec4
             | CmdRotate Vec4 Double
             | CmdScale Double Double Double
             | CmdPushTransform
             | CmdPopTransform
             | CmdDirectional Double Double Double Double Double Double
             | CmdPoint Double Double Double Double Double Double
             | CmdAttenuation Double Double Double
             | CmdAmbient Double Double Double
             | CmdDiffuse Double Double Double
             | CmdSpecular Double Double Double
             | CmdShininess Double
             | CmdEmission Double Double Double
             | Comment
               deriving (Show)

spec :: Parser [Command]
spec = do
  params <- many command
  eof
  return params

command :: Parser Command
command =  size <|> maxdepth <|> output <|>
           camera <|>
           sphere <|> maxverts <|> vertex <|> tri <|>
           translate <|> rotate <|> scale <|> pushTransform <|> popTransform <|>
           directional <|> point <|> attenuation <|> ambient <|>
           diffuse <|> specular <|> shininess <|> emission <|>
           comment

comment :: Parser Command
comment = do
  try $ string "#"
  many (noneOf "\n")
  eol
  return Comment

output :: Parser Command
output = do
  try $ string "output"
  whitespace
  filename <- many (noneOf "\n")
  eol
  return $ CmdOutput filename

number = many1 digit

nat = read <$> number

int = pos <|> neg <|> number
  where pos = char '+' *> number
        neg = (:) <$> char '-' <*> number

double = fmap read $ (++) <$> int <*> decimal
  where decimal = option "" $ (:) <$> char '.' <*> number

maxdepth :: Parser Command
maxdepth = do
  try $ string "maxdepth"
  whitespace
  d <- nat
  eol
  return $ CmdMaxdepth d

size :: Parser Command
size = do
  try $ string "size"
  whitespace
  w <- nat
  whitespace
  h <- nat
  eol
  return $ CmdSize w h

camera :: Parser Command
camera = do
  try $ string "camera"
  whitespace
  eyex <- double
  whitespace
  eyey <- double
  whitespace
  eyez <- double
  whitespace
  targetx <- double
  whitespace
  targety <- double
  whitespace
  targetz <- double
  whitespace
  upx <- double
  whitespace
  upy <- double
  whitespace
  upz <- double
  whitespace
  fieldofview <- double
  eol
  return $ CmdCamera eyex eyey eyez targetx targety targetz upx upy upz fieldofview

sphere :: Parser Command
sphere = do
  try $ string "sphere"
  whitespace
  x <- double
  whitespace
  y <- double
  whitespace
  z <- double
  whitespace
  r <- double
  eol
  return $ CmdSphere x y z r

maxverts :: Parser Command
maxverts = do
  try $ string "maxverts"
  whitespace
  n <- nat
  eol
  return $ CmdMaxVerts n

vertex :: Parser Command
vertex = do
  try $ string "maxverts"
  whitespace
  x <- double
  whitespace
  y <- double
  whitespace
  z <- double
  eol
  return $ CmdVertex x y z

tri :: Parser Command
tri = do
  try $ string "tri"
  whitespace
  v1 <- nat
  whitespace
  v2 <- nat
  whitespace
  v3 <- nat
  eol
  return $ CmdTri v1 v2 v3

translate :: Parser Command
translate = do
  try $ string "translate"
  whitespace
  x <- double
  whitespace
  y <- double
  whitespace
  z <- double
  eol
  return $ CmdTranslate (Vec4 x y z 0)

rotate :: Parser Command
rotate = do
  try $ string "rotate"
  whitespace
  x <- double
  whitespace
  y <- double
  whitespace
  z <- double
  whitespace
  a <- double
  eol
  return $ CmdRotate (Vec4 x y z 0) a

scale :: Parser Command
scale = do
  try $ string "scale"
  whitespace
  x <- double
  whitespace
  y <- double
  whitespace
  z <- double
  eol
  return $ CmdScale x y z

pushTransform :: Parser Command
pushTransform = do
  try $ string "pushTransform"
  eol
  return CmdPushTransform

popTransform :: Parser Command
popTransform = do
  try $ string "popTransform"
  eol
  return CmdPopTransform

directional :: Parser Command
directional = do
  try $ string "directional"
  whitespace
  x <- double
  whitespace
  y <- double
  whitespace
  z <- double
  whitespace
  r <- double
  whitespace
  g <- double
  whitespace
  b <- double
  eol
  return $ CmdDirectional x y z r g b

point :: Parser Command
point = do
  try $ string "point"
  whitespace
  x <- double
  whitespace
  y <- double
  whitespace
  z <- double
  whitespace
  r <- double
  whitespace
  g <- double
  whitespace
  b <- double
  eol
  return $ CmdPoint x y z r g b

attenuation :: Parser Command
attenuation = do
  try $ string "attenuation"
  whitespace
  const <- double
  whitespace
  linear <- double
  whitespace
  quadratic <- double
  eol
  return $ CmdAttenuation const linear quadratic

ambient :: Parser Command
ambient = do
  try $ string "ambient"
  whitespace
  r <- double
  whitespace
  g <- double
  whitespace
  b <- double
  eol
  return $ CmdAmbient r g b

diffuse :: Parser Command
diffuse = do
  try $ string "diffuse"
  whitespace
  r <- double
  whitespace
  g <- double
  whitespace
  b <- double
  eol
  return $ CmdDiffuse r g b

specular :: Parser Command
specular = do
  try $ string "specular"
  whitespace
  r <- double
  whitespace
  g <- double
  whitespace
  b <- double
  eol
  return $ CmdSpecular r g b

shininess :: Parser Command
shininess = do
  try $ string "shininess"
  whitespace
  s <- double
  eol
  return $ CmdShininess s

emission :: Parser Command
emission = do
  try $ string "emission"
  whitespace
  r <- double
  whitespace
  g <- double
  whitespace
  b <- double
  eol
  return $ CmdEmission r g b

eol = void (char '\n') <|> eof
whitespace = char ' '

test :: IO ()
test = parseTest spec "size 256 256\ncamera 1 0.2 1 1 1 1 20\noutput out.png"

testFile :: IO (Either ParseError [Command])
testFile = parseFromFile spec "test.txt"

data Camera = Camera { eye :: Point3, target :: Point3, up :: Vec4, fov :: Double } deriving Show
data Size = Size Int Int deriving Show
-- kd = diffuse, ks = specular, sh = shininess, ke = emission
data Material = Material { kd :: Color, ks :: Color, sh :: Double, ke :: Color } deriving Show
data LightAttenuation = LightAttenuation { constantLightAttenuation :: Double, linearLightAttenuation :: Double, quadraticLightAttenuation :: Double } deriving Show
data Light = DirectionalLight Vec4 Color | PointLight Point3 Color deriving Show
data Rig = Rig { ka :: Color, att :: LightAttenuation, lights :: [Light] } deriving Show
data Params = Params { cam :: Camera
                     , sz :: Size
                     , out :: String
                     , objs :: [(Shape, Material)]
                     , rig :: Rig
                     } deriving (Show)

params :: [Command] -> Params
params cmds = let defaultMaterial = Material { kd = makeColor 0 0 0 1, ks = makeColor 0 0 0 1, sh = 0, ke = makeColor 0 0 0 1 }
                  defaultParams = Params { cam = Camera { eye = Point3 0 0 0, target = Point3 0 0 (-2), up = Vec4 0 (-1) 0 0, fov = 20.0 }
                                         , sz  = Size 100 100
                                         , out = "default.png"
                                         , objs = []
                                         , rig = Rig { ka = makeColor 0.2 0.2 0.2 1
                                                     , att = LightAttenuation { constantLightAttenuation = 1, linearLightAttenuation = 0, quadraticLightAttenuation = 0 }
                                                     , lights = []
                                                     }
                                         }
              in buildParam cmds [identity4] defaultMaterial defaultParams
                  where buildParam :: [Command] -> [Mat4] -> Material -> Params -> Params
                        buildParam [] xforms mat p = p
                        buildParam (c:cs) xforms mat p =
                          let cam' = case c of
                                       CmdCamera x y z tx ty tz upx upy upz fieldofview -> Camera { eye = Point3 x y z, target = Point3 tx ty tz, up = normalize $ Vec4 upx upy upz 0, fov = fieldofview }
                                       otherwise                                        -> cam p
                              sz'  = case c of
                                       CmdSize w h -> Size w h
                                       otherwise   -> sz p
                              out' = case c of
                                       CmdOutput f -> f
                                       otherwise   -> out p
                              objs' = case c of
                                       CmdSphere x y z r -> (Sphere { center = Point3 x y z, radius = r, transform = head xforms }, mat): objs p
                                       otherwise -> objs p
                              mat' = case c of
                                       CmdDiffuse r g b  -> Material { kd = makeColor (double2Float r) (double2Float g) (double2Float b) 1, ks = ks mat, sh = sh mat, ke = ke mat }
                                       CmdSpecular r g b -> Material { kd = kd mat, ks = makeColor (double2Float r) (double2Float g) (double2Float b) 1, sh = sh mat, ke = ke mat }
                                       CmdShininess s    -> Material { kd = kd mat, ks = ks mat, sh = s, ke = ke mat }
                                       CmdEmission r g b -> Material { kd = kd mat, ks = ks mat, sh = sh mat, ke = makeColor (double2Float r) (double2Float g) (double2Float b) 1 }
                                       otherwise -> mat
                              rig' = case c of CmdAmbient r g b -> Rig { ka = makeColor (double2Float r) (double2Float g) (double2Float b) 1, att = att $ rig p, lights = lights $ rig p }
                                               CmdAttenuation c l q -> Rig { ka = ka $ rig p, att = LightAttenuation { constantLightAttenuation = c, linearLightAttenuation = l, quadraticLightAttenuation = q }, lights = lights $ rig p }
                                               CmdDirectional x y z r g b -> Rig { ka = ka $ rig p, att = att $ rig p, lights = DirectionalLight (Vec4 x y z 0) (makeColor (double2Float r) (double2Float g) (double2Float b) 1): (lights $ rig p) }
                                               CmdPoint x y z r g b -> Rig { ka = ka $ rig p, att = att $ rig p, lights = PointLight (Point3 x y z) (makeColor (double2Float r) (double2Float g) (double2Float b) 1): (lights $ rig p) }
                                               otherwise -> rig p
                              xforms' = case c of CmdPushTransform -> (head xforms):xforms
                                                  CmdPopTransform -> tail xforms
                                                  CmdTranslate t -> ((head xforms) * (translationmat t)):(tail xforms)
                                                  CmdRotate axis angle -> ((head xforms) * (rotationmat axis angle)):(tail xforms)
                                                  CmdScale x y z -> ((head xforms) * (scalemat x y z)):(tail xforms)
                                                  otherwise -> xforms
                          in buildParam cs xforms' mat' Params { cam = cam'
                                                               , sz = sz'
                                                               , out = out'
                                                               , objs = objs'
                                                               , rig = rig'
                                                               }

testParams :: Params
testParams = params commands
  where commands = case parse spec "" "size 256 256\ncamera 1 0.2 1 1 1 1 20\noutput out.png" of
                     Left err -> []
                     Right cs -> cs

testParamsFromFile :: IO (Params)
testParamsFromFile = params <$> commands
  where commands = parseFromFile spec "test.txt" >>= either empty return
        empty err = return []

paramsFromFile :: String -> IO (Params)
paramsFromFile filename = params <$> commands
  where commands = parseFromFile spec filename >>= either empty return
        empty err = return []
