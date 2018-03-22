import System.IO ()
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

type Point3D = (Float, Float, Float)

move :: Point3D -> Point3D -> Point3D
move (dx, dy, dz) (x, y, z) = (x + dx, y + dy, z + dz)

rotateX :: Float -> Point3D -> Point3D
rotateX degree (x, y, z) = (x, c*y + s*z, -s*y + c*z)
    where radiant = normalizeAngle $ degToRad degree
          c = cos radiant
          s = sin radiant

rotateZ :: Float -> Point3D -> Point3D
rotateZ degree (x, y, z) = (c*x + s*y, -s*x + c*y, z)
    where radiant = normalizeAngle $ degToRad degree
          c = cos radiant
          s = sin radiant

project :: Point3D -> Point
project (x0, y0, z0) = (x', y')
    where (ax, ay, az) = (x0, y0, z0)
          (cx, cy, cz) = (0.0, 0.0, 0.0)
          (x, y, z)    = (ax - cx, ay - cy, az - cz)
          (a, b, c)    = (degToRad 0.0, degToRad 0.0, degToRad 0.0)
          (ex, ey, ez) = (0.0, 0.0, 300.0)
          (dx, dy, dz) = (cosy*(sinz*y+cosz*x)-siny*z,
                          sinx*(cosy*z+siny*(sinz*y+cosz*x))+cosx*(cosz*y-sinz*x),
                          cosx*(cosy*z+siny*(sinz*y+cosz*x))-sinx*(cosz*y-sinz*x))
          (cosx, sinx) = (cos a, sin a)
          (cosy, siny) = (cos b, sin b)
          (cosz, sinz) = (cos c, sin c)
          x' = ez/dz*dx - ex
          y' = ez/dz*dy - ey

type PointGenerator = (Float ->
                       Point3D ->
                       Point3D ->
                       Point3D ->
                       Point3D ->
                       Point3D)

cubic :: PointGenerator
cubic t cp0 cp1 cp2 cp3 = (xnew, ynew, znew)
   where (x0, y0, z0) = cp0
         (x1, y1, z1) = cp1
         (x2, y2, z2) = cp2
         (x3, y3, z3) = cp3
         a = 1.0 - t
         func = (\foo0 foo1 foo2 foo3 -> a^(3::Int)*foo0 +
                3.0*a^(2::Int)*t*foo1 +
                3.0*a*t^(2::Int)*foo2 +
                t^(3::Int)*foo3)
         xnew = func x0 x1 x2 x3
         ynew = func y0 y1 y2 y3
         znew = func z0 z1 z2 z3

surfacePoint :: Float -> Float -> [Point3D] -> Point3D
surfacePoint s t ctrls = cubic t cp0 cp1 cp2 cp3
    where cp0 = cubic s (ctrls!!0) (ctrls!!1) (ctrls!!2) (ctrls!!3)
          cp1 = cubic s (ctrls!!4) (ctrls!!5) (ctrls!!6) (ctrls!!7)
          cp2 = cubic s (ctrls!!8) (ctrls!!9) (ctrls!!10) (ctrls!!11)
          cp3 = cubic s (ctrls!!12) (ctrls!!13) (ctrls!!14) (ctrls!!15)

patch :: Int -> [Point3D] -> [Point3D]
patch res ctrlPts = map (\(s, t) -> surfacePoint s t ctrlPts) $
                    concat $ map (\a -> zip oneDimList $ repeat a) oneDimList
                    where step = 1.0 / fromIntegral res::Float
                          oneDimList = take res $ iterate (+step) 0.0

dot :: Color -> Point -> Picture
dot col (x, y) = Color col $ translate x y $ thickCircle 0.5 1.0

blackDot :: Point -> Picture
blackDot (x, y) = dot black (x, y)

controlPoints :: [Point3D]
controlPoints = [(-150.0,    0.0, -150.0),
                 (-150.0,  190.0,  -50.0),
                 (-150.0,    0.0,   50.0),
                 (-150.0,   50.0,  150.0),

                 ( -50.0,    0.0, -150.0),
                 ( -50.0,   70.0,  -50.0),
                 ( -50.0,    0.0,   50.0),
                 ( -50.0, -100.0,  150.0),

                 (  50.0,    0.0, -150.0),
                 (  50.0,   70.0,  -50.0),
                 (  50.0,    0.0,   50.0),
                 (  50.0, -100.0,  150.0),

                 ( 150.0,    0.0, -150.0),
                 ( 150.0,   70.0,  -50.0),
                 ( 150.0,    0.0,   50.0),
                 ( 150.0, -100.0,  150.0)]

frame :: Float -> Picture
frame seconds = pictures $ map blackDot projectedPoints
                where offset = move (0.0, 0.0, 400.0)
                      rotate' = rotateX (seconds * 35.0) . rotateZ (seconds * 45.0)
                      transform' = offset . rotate'
                      transformedPoints = map transform' $ patch 40 controlPoints
                      projectedPoints = map project transformedPoints

windowPosition :: (Int, Int)
windowPosition = (20, 200)

windowSize :: (Int, Int)
windowSize = (800, 600)

window :: Display
window = InWindow "Cubic Bézier-Surface in Haskell" windowSize windowPosition

main :: IO ()
main = animate window white frame
