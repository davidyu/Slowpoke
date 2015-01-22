module Math where

import Debug.Trace

toRad :: Double -> Double
toRad deg = deg * pi / 180

data Vec4 = Vec4 Double Double Double Double deriving (Show)

makeVec4FromList [x, y, z, w] = Vec4 x y z w
makeVec4FromList _ = undefined -- error

class Ops a where
  (|*|) :: Double -> a -> a -- scalar multiplication
  dot :: a -> a -> Double
  cross :: a -> a -> a

instance Num Vec4
  where
    (Vec4 ax ay az aw) + (Vec4 bx by bz bw) = Vec4 (ax + bx) (ay + by) (az + bz) (aw + bw)
    (Vec4 ax ay az aw) - (Vec4 bx by bz bw) = Vec4 (ax - bx) (ay - by) (az - bz) (aw - bw)
    a * b                                   = a `cross` b
    negate  (Vec4 x y z w)                  = Vec4 (-x) (-y) (-z) w
    abs                                     = undefined
    signum                                  = undefined
    fromInteger                             = undefined

instance Ops Vec4 where
  a |*| Vec4 x y z w = Vec4 (a * x) (a * y) (a * z) (a * w)
  dot (Vec4 ax ay az aw) (Vec4 bx by bz bw) = ax * bx + ay * by + az * bz + aw * bw
  cross (Vec4 ax ay az aw) (Vec4 bx by bz bw) = Vec4 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx) 0

lensq (Vec4 x y z _) = x * x + y * y + z * z

len (Vec4 x y z _) = sqrt( x * x + y * y + z * z )

getX (Vec4 x _ _ _ ) = x
getY (Vec4 _ y _ _ ) = y
getZ (Vec4 _ _ z _ ) = z
getW (Vec4 _ _ _ w ) = w

get n (Vec4 x y z w)
  | n == 0 = x
  | n == 1 = y
  | n == 2 = z
  | n == 3 = w

normalize (Vec4 x y z w) = Vec4 (x/l) (y/l) (z/l) w where l = len (Vec4 x y z w)

tensor :: Vec4 -> Mat4
tensor (Vec4 x y z w) = Mat4 { m00 = x * x, m01 = x * y, m02 = x * z, m03 = 0
                             , m10 = y * x, m11 = y * y, m12 = y * z, m13 = 0
                             , m20 = z * x, m21 = z * y, m22 = z * z, m23 = 0
                             , m30 = 0    , m31 = 0    , m32 = 0    , m33 = 0
                             }

data Point3 = Point3 Double Double Double deriving(Show)

instance Num Point3
  where
    (Point3 ax ay az) + (Point3 bx by bz) = Point3 (ax + bx) (ay + by) (az + bz)
    (Point3 ax ay az) - (Point3 bx by bz) = Point3 (ax - bx) (ay - by) (az - bz)
    (*)                                   = trace ("Point3 dot product") undefined
    negate (Point3 x y z)                 = Point3 (-x) (-y) (-z)
    abs                                   = undefined
    signum                                = undefined
    fromInteger                           = undefined

instance Ops Point3 where
  a |*| Point3 x y z = Point3 (a * x) (a * y) (a * z)
  dot (Point3 ax ay az) (Point3 bx by bz) = ax * bx + ay * by + az * bz
  cross = trace ("Point3 cross") undefined

fromPoint :: Point3 -> Vec4
fromPoint (Point3 x y z) = Vec4 x y z 1

toPoint :: Vec4 -> Point3
toPoint (Vec4 x y z _) = Point3 x y z

data Mat4 = Mat4 { m00 :: Double, m10 :: Double, m20 :: Double, m30 :: Double
                 , m01 :: Double, m11 :: Double, m21 :: Double, m31 :: Double
                 , m02 :: Double, m12 :: Double, m22 :: Double, m32 :: Double
                 , m03 :: Double, m13 :: Double, m23 :: Double, m33 :: Double
                 }

instance Show Mat4 where
  show m = (show $ m00 m) ++ "\t" ++ (show $ m01 m) ++ "\t" ++ (show $ m02 m) ++ "\t" ++ (show $ m03 m) ++ "\n" ++
           (show $ m10 m) ++ "\t" ++ (show $ m11 m) ++ "\t" ++ (show $ m12 m) ++ "\t" ++ (show $ m13 m) ++ "\n" ++
           (show $ m20 m) ++ "\t" ++ (show $ m21 m) ++ "\t" ++ (show $ m22 m) ++ "\t" ++ (show $ m23 m) ++ "\n" ++
           (show $ m30 m) ++ "\t" ++ (show $ m31 m) ++ "\t" ++ (show $ m32 m) ++ "\t" ++ (show $ m33 m)

makeMat4 a b c d e f g h i j k l m n o p =
  Mat4 { m00 = a, m10 = b, m20 = c, m30 = d
       , m01 = e, m11 = f, m21 = g, m31 = h
       , m02 = i, m12 = j, m22 = k, m32 = l
       , m03 = m, m13 = n, m23 = o, m33 = p }

makeMat4RowMajor a b c d e f g h i j k l m n o p =
  Mat4 { m00 = a, m10 = e, m20 = i, m30 = m
       , m01 = b, m11 = f, m21 = j, m31 = n
       , m02 = c, m12 = g, m22 = k, m32 = o
       , m03 = d, m13 = h, m23 = l, m33 = p }

mat4 = makeMat4  1 0 0 0
                 0 1 0 0
                 0 0 1 0
                 0 0 0 1

identity4 = mat4
identity3 = makeMat4 1 0 0 0
                     0 1 0 0
                     0 0 1 0
                     0 0 0 0

-- convenience
makeMat4FromList [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p] = makeMat4 a b c d e f g h i j k l m n o p
makeMat4FromList lst = error ("expecting 16 elements in list, got " ++ (show $ length lst))

makeMat4RowMajorFromList [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p] = makeMat4RowMajor a b c d e f g h i j k l m n o p
makeMat4RowMajorFromList lst = error ("expecting 16 elements in list, got " ++ (show $ length lst))

makeMat4FromRowVecs [r1, r2, r3, r4]
  = let Vec4 a b c d = r1
        Vec4 e f g h = r2
        Vec4 i j k l = r3
        Vec4 m n o p = r4
    in makeMat4RowMajor a b c d e f g h i j k l m n o p
makeMat4FromRowVecs _ = undefined

-- convenience accessors
tx = m03
ty = m13
tz = m23
scx = m00
scy = m11
scz = m22
r00 = m00
r10 = m10
r20 = m20
r01 = m01
r11 = m11
r21 = m21
r02 = m02
r12 = m12
r22 = m22

-- array-like accessors
m r c mat
  = get c $ rowvec r mat


-- raw row and column accessors
row n mat
  | n == 0 = [m00 mat, m01 mat, m02 mat, m03 mat]
  | n == 1 = [m10 mat, m11 mat, m12 mat, m13 mat]
  | n == 2 = [m20 mat, m21 mat, m22 mat, m23 mat]
  | n == 3 = [m30 mat, m31 mat, m32 mat, m33 mat]

col n mat
  | n == 0 = [m00 mat, m10 mat, m20 mat, m30 mat]
  | n == 1 = [m01 mat, m11 mat, m21 mat, m31 mat]
  | n == 2 = [m02 mat, m12 mat, m22 mat, m32 mat]
  | n == 3 = [m03 mat, m13 mat, m23 mat, m33 mat]

-- w/ filtering
rowWithoutCol r c mat
  = let rr = row r mat in (take c rr) ++ (drop (c+1) rr) ++ [0]

-- vector row and column accessors
rowvec n mat
  | n == 0 = Vec4 (m00 mat) (m01 mat) (m02 mat) (m03 mat)
  | n == 1 = Vec4 (m10 mat) (m11 mat) (m12 mat) (m13 mat)
  | n == 2 = Vec4 (m20 mat) (m21 mat) (m22 mat) (m23 mat)
  | n == 3 = Vec4 (m30 mat) (m31 mat) (m32 mat) (m33 mat)

colvec n mat
  | n == 0 = Vec4 (m00 mat) (m10 mat) (m20 mat) (m30 mat)
  | n == 1 = Vec4 (m01 mat) (m11 mat) (m21 mat) (m31 mat)
  | n == 2 = Vec4 (m02 mat) (m12 mat) (m22 mat) (m32 mat)
  | n == 3 = Vec4 (m03 mat) (m13 mat) (m23 mat) (m33 mat)

-- NOTE: row major!
mat4ToList m = concatMap (\r -> row r m) [0..3]

mat4ToListColMajor m = concatMap (\c -> col c m) [0..3]

-- TODO check perf/codegen of Vec4 vs lists for multiplication here
matmul :: Mat4 -> Mat4 -> Mat4
matmul lhs rhs = makeMat4RowMajorFromList $ concatMap (computeRow lhs rhs) [0..3]
  where prod = zipWith (*) -- is there a name for this?
        computeRow lhs rhs r = map (\c -> sum $ prod (row r lhs) (col c rhs)) [0..3]

instance Ops Mat4 where
  a |*| m = makeMat4RowMajorFromList $ map (* a) (mat4ToList m)
  dot = undefined
  cross = undefined

instance Num Mat4
  where
    a + b       = makeMat4RowMajorFromList $ zipWith (+) (mat4ToList a) (mat4ToList b)
    a - b       = makeMat4RowMajorFromList $ zipWith (-) (mat4ToList a) (mat4ToList b)
    a * b       = a `matmul` b
    negate a    = makeMat4RowMajorFromList $ map (negate) (mat4ToList a)
    abs         = undefined
    signum      = undefined
    fromInteger = undefined

matvec :: Mat4 -> Vec4 -> Vec4
matvec lhs rhs = makeVec4FromList $ map (dot rhs) (map (\r -> rowvec r lhs) [0..3])

vecmat :: Vec4 -> Mat4 -> Vec4
vecmat lhs rhs = makeVec4FromList $ map (dot lhs) (map (\c -> colvec c rhs) [0..3])

rotationmat :: Vec4 -> Double -> Mat4
rotationmat (Vec4 x y z _) angle = (cos r |*| identity3) + (sin r |*| crossmat) + ((1 - cos r) |*| tensor (Vec4 x y z 0)) + w
  where r = toRad angle
        crossmat = makeMat4RowMajor   0  (-z)   y  0
                                      z    0  (-x) 0
                                    (-y)   x    0  0
                                      0    0    0  0
        w = makeMat4RowMajor 0 0 0 0 -- my own hack to make sure m33 is 1
                             0 0 0 0
                             0 0 0 0
                             0 0 0 1

scalemat :: Double -> Double -> Double -> Mat4
scalemat x y z = Mat4 { m00 = x, m10 = 0, m20 = 0, m30 = 0
                      , m01 = 0, m11 = y, m21 = 0, m31 = 0
                      , m02 = 0, m12 = 0, m22 = z, m32 = 0
                      , m03 = 0, m13 = 0, m23 = 0, m33 = 1
                      }

translationmat :: Vec4 -> Mat4
translationmat (Vec4 x y z _) = Mat4 { m00 = 1, m10 = 0, m20 = 0, m30 = 0
                                     , m01 = 0, m11 = 1, m21 = 0, m31 = 0
                                     , m02 = 0, m12 = 0, m22 = 1, m32 = 0
                                     , m03 = x, m13 = y, m23 = z, m33 = 1
                                     }


det3 Mat4 { m00 = a, m01 = b, m02 = c, m03 = _
          , m10 = d, m11 = e, m12 = f, m13 = _
          , m20 = g, m21 = h, m22 = i, m23 = _
          , m30 = _, m31 = _, m32 = _, m33 = _
          } = a*e*i + b*f*g + c*d*h - c*e*g - b*d*i - a*f*h

det mat
  = m00 mat * (det3 $ makeMat4RowMajorFromList (concatMap (\r -> rowWithoutCol r 0 mat) [1,2,3] ++ [0,0,0,0]))
  - m01 mat * (det3 $ makeMat4RowMajorFromList (concatMap (\r -> rowWithoutCol r 1 mat) [1,2,3] ++ [0,0,0,0]))
  + m02 mat * (det3 $ makeMat4RowMajorFromList (concatMap (\r -> rowWithoutCol r 2 mat) [1,2,3] ++ [0,0,0,0]))
  - m03 mat * (det3 $ makeMat4RowMajorFromList (concatMap (\r -> rowWithoutCol r 3 mat) [1,2,3] ++ [0,0,0,0]))

tr mat
  = (m00 mat) + (m11 mat) + (m22 mat) + (m33 mat)

-- caley hamilton decomposition to find inverse of a matrix
caleyhamilton :: Mat4 -> Mat4
caleyhamilton a
  = let d = det a
        t = tr a
        t2 = tr (a*a)
        t3 = tr (a*a*a)
        a' = (1/6) * ((t*t*t) - (3*t*t2) + (2*t3))
        b' = (1/2) * ((t*t) - t2)
    in if d /= 0 then (1/d) |*| (a' |*| identity4 - b' |*| a + (t|*|(a*a)) - (a*(a*a)))
                 else error "matrix is singular"

inv :: Mat4 -> Mat4
inv = caleyhamilton

data Ray = Ray { start :: Point3
               , direction :: Vec4
               } deriving (Show)

-- applies transform t to ray r
-- make sure we are applying mat4 without translation to vec4
apply :: Mat4 -> Ray -> Ray
apply t r = Ray { start = (start r) + (Point3 (tx t) (ty t) (tz t)),
                  direction = vecmat (direction r) t }

data Shape = Sphere { center :: Point3 , radius :: Double , transform :: Mat4 }
           | Triangle { v1 :: Point3 , v2 :: Point3 , v3 :: Point3 }
             deriving (Show)

data IntersectResult = NoHit | Hit [(Double, Point3, Vec4)]

intersect :: Ray -> Shape -> IntersectResult
intersect ray Sphere { center = ct, radius = r, transform = xf } =
  let  Ray {start = s, direction = v} = apply (inv xf) ray
       a = lensq v -- == 1
       b = 2 * dot (toPoint v) (s - ct)
       c = lensq (fromPoint s - fromPoint ct) - r * r
  in getRaySphereXSection a b c (b * b - 4 * a * c) ct s v
     where getRaySphereXSection a b c discr ct s v
              | discr < (-epsilon) = NoHit
              | discr > epsilon  =
                let t1 = ( ( -b ) - sqrt discr ) / ( 2 * a )
                    t2 = ( ( -b ) + sqrt discr ) / ( 2 * a )
                    p1 = s + t1 |*| toPoint v
                    p2 = s + t2 |*| toPoint v
                    n1 = normalize $ fromPoint (p1 - ct)
                    n2 = normalize $ fromPoint (p2 - ct)
                in trace ("normal: " ++ show n1 ++ "\t\tp: " ++ show p1) (Hit [ (t1, p1, n1), (t2, p2, n2)])
              | otherwise =
                let t = ( -b ) / ( 2 * a )
                    p = s + t |*| toPoint v
                    n = normalize $ fromPoint (p - ct)
                in trace (show n) (Hit [ (t, p, n) ])
           epsilon = 0.0001
intersect ray Triangle { v1 = v1, v2 = v2, v3 = v3 } =
  let Ray { start = o, direction = d } = ray
      e1 = v2 - v1
      e2 = v3 - v1
      n = toPoint $ (fromPoint e1) `cross` (fromPoint e2)
      s = o - v1
      m = toPoint $ (fromPoint s) `cross` d
      Point3 t u v = (1/(-(n `dot` (toPoint d)))) |*| Point3 (n `dot` s) (m `dot` e2) ((-m) `dot` e1)
  in if inRange u v && t > 0 then Hit [(t, o + t |*| toPoint d, normalize $ fromPoint n)] else NoHit
    where inRange u v = u >= 0.0 && v >= 0.0 && (u + v) <= 1.0

