import System.IO ()
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

window :: Display
window = InWindow "3D in 2D with Haskell" (600, 450) (20, 20)


type Point3D = (Float, Float, Float)

cube0 :: [Point3D]
cube0 = [( 150.0,  150.0,  150.0),
         ( 150.0, -150.0,  150.0),
         (-150.0,  150.0,  150.0),
         (-150.0, -150.0,  150.0),
         ( 150.0,  150.0, -150.0),
         ( 150.0, -150.0, -150.0),
         (-150.0,  150.0, -150.0),
         (-150.0, -150.0, -150.0)]

cube1 :: [Point3D]
cube1 = [( 125.0,  125.0,  125.0),
         ( 125.0, -125.0,  125.0),
         (-125.0,  125.0,  125.0),
         (-125.0, -125.0,  125.0),
         ( 125.0,  125.0, -125.0),
         ( 125.0, -125.0, -125.0),
         (-125.0,  125.0, -125.0),
         (-125.0, -125.0, -125.0)]

cube2 :: [Point3D]
cube2 = [( 100.0,  100.0,  100.0),
         ( 100.0, -100.0,  100.0),
         (-100.0,  100.0,  100.0),
         (-100.0, -100.0,  100.0),
         ( 100.0,  100.0, -100.0),
         ( 100.0, -100.0, -100.0),
         (-100.0,  100.0, -100.0),
         (-100.0, -100.0, -100.0)]

p0' :: Point3D
p0' = (-150.0, -150.0, 150.0)

p1' :: Point3D
p1' = (-200.0, 50.0, 225.0)

p2' :: Point3D
p2' = (50.0, -200.0, -225.0)

p3' :: Point3D
p3' = (-150.0, -150.0, -150.0)

p0'' :: Point3D
p0'' = (-125.0, -125.0, 125.0)

p1'' :: Point3D
p1'' = (-150.0, 75.0, 175.0)

p2'' :: Point3D
p2'' = (75.0, -150.0, -175.0)

p3'' :: Point3D
p3'' = (-125.0, -125.0, -125.0)

p0''' :: Point3D
p0''' = (-100.0, -100.0, 100.0)

p1''' :: Point3D
p1''' = (-125.0, 75.0, 125.0)

p2''' :: Point3D
p2''' = (75.0, -125.0, -125.0)

p3''' :: Point3D
p3''' = (-100.0, -100.0, -100.0)

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
          (dx, dy, dz) = (cosy*(sinz*y + cosz*x) - siny*z,
                          sinx*(cosy*z + siny*(sinz*y + cosz*x)) +
                          cosx*(cosz*y - sinz*x),
                          cosx*(cosy*z + siny*(sinz*y + cosz*x)) -
                          sinx*(cosz*y - sinz*x)) 
          (cosx, sinx) = (cos a, sin a)
          (cosy, siny) = (cos b, sin b)
          (cosz, sinz) = (cos c, sin c)
          x' = ez/dz*dx - ex
          y' = ez/dz*dy - ey

move :: Point3D -> Point3D -> Point3D
move (dx, dy, dz) (x, y, z) = (x + dx, y + dy, z + dz)

toWire :: Path -> Path
toWire [] = []
toWire xs = [p0, p1, p3, p2, p0, p4, p5, p7, p6, p4]
    where p0 = xs !! 0
          p1 = xs !! 1
          p2 = xs !! 2
          p3 = xs !! 3 
          p4 = xs !! 4 
          p5 = xs !! 5 
          p6 = xs !! 6 
          p7 = xs !! 7 

frame :: Float -> Picture
frame seconds = pictures [Color black $ line $ toWire imageSpaceCoords0,
                          Color black $ line $ toWire imageSpaceCoords1,
                          Color black $ line $ toWire imageSpaceCoords2,
                          Color red $ line spline0,
                          Color green $ line spline1,
                          Color blue $ line spline2]
                where offset = move (0.0, 0.0, 400.0)
                      rotate' = rotateX (seconds * 35.0) .
                                rotateZ (seconds * 45.0)
                      rotate'' = rotateX ((seconds-0.05) * 37.0) .
                                 rotateZ ((seconds-0.05) * 47.0)
                      rotate''' = rotateX ((seconds-0.09) * 30.0) .
                                  rotateZ ((seconds-0.09) * 40.0)
                      moved0 = map (offset . rotate') cube0
                      moved1 = map (offset . rotate'') cube1
                      moved2 = map (offset . rotate''') cube2
                      imageSpaceCoords0 = map project moved0
                      imageSpaceCoords1 = map project moved1
                      imageSpaceCoords2 = map project moved2
                      transform' = offset . rotate'
                      transform'' = offset . rotate''
                      transform''' = offset . rotate'''
                      [a, b, c, d] = map transform' [p0', p1', p2', p3']
                      [p, q, r, s] = map transform'' [p0'', p1'', p2'', p3'']
                      [i, j, k, l] = map transform''' [p0''', p1''', p2''', p3''']
                      spline0 = curve cubic 50 a b c d
                      spline1 = curve cubic 50 p q r s
                      spline2 = curve cubic 50 i j k l

main :: IO ()
main = animate window white frame

type PointGenerator = (Float ->
                       Point3D ->
                       Point3D ->
                       Point3D ->
                       Point3D ->
                       Point3D)

curve :: PointGenerator ->
         Int ->
         Point3D ->
         Point3D ->
         Point3D ->
         Point3D ->
         Path
curve pg res cp0 cp1 cp2 cp3 = map (\t -> project $ pg t cp0 cp1 cp2 cp3) $
                               take (res + 1) $ iterate (+ step) 0.0
                               where step = 1.0 / fromIntegral res::Float

cubic :: PointGenerator
cubic t cp0 cp1 cp2 cp3 = (xnew, ynew, znew)
   where (x0, y0, z0) = cp0
         (x1, y1, z1) = cp1 
         (x2, y2, z2) = cp2
         (x3, y3, z3) = cp3 
         a = 1.0 - t
         xnew = a^(3::Int)*x0 +
                3.0*a^(2::Int)*t*x1 +
                3.0*a*t^(2::Int)*x2 +
                t^(3::Int)*x3
         ynew = a^(3::Int)*y0 +
                3.0*a^(2::Int)*t*y1 +
                3.0*a*t^(2::Int)*y2 +
                t^(3::Int)*y3
         znew = a^(3::Int)*z0 +
                3.0*a^(2::Int)*t*z1 +
                3.0*a*t^(2::Int)*z2 +
                t^(3::Int)*z3
