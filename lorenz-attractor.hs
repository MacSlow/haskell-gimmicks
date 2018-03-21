-- ghc -O2 -dynamic -threaded -rtsopts -Wall -Werror --make lorenz-attractor
-- sstrip lorenz-attractor

import System.IO ()
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

window :: Display
window = InWindow "Lorenz-Attractor with Haskell" (600, 450) (20, 20)

type Point3D = (Float, Float, Float)

lorenzPoints :: Point3D -> [Point3D]
lorenzPoints (x, y, z) = (x', y', z') : lorenzPoints (x', y', z')
    where dt = 0.005
          sigma = 10.0
          rho = 28.0
          beta = 8.0/3.0
          dx = sigma*(y - x)
          dy = x*(rho - z) - y
          dz = x*y - beta*z
          x' = x + dx * dt
          y' = y + dy * dt
          z' = z + dz * dt

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

coordSys :: Float -> (Float, Float, Float) -> Float -> Float -> Picture
coordSys s offset' angle0 angle1 = pictures [Color red $ line one,
                                             Color green $ line two,
                                             Color blue $ line three]
    where offset = move offset'
          rotate' = rotateX (angle0) . rotateZ (angle1)
          transform = offset . rotate'
          one = map project $ map transform [(0.0, 0.0, 0.0), (s, 0.0, 0.0)]
          two = map project $ map transform [(0.0, 0.0, 0.0), (0.0, s, 0.0)]
          three = map project $ map transform [(0.0, 0.0, 0.0), (0.0, 0.0, s)]

move :: Point3D -> Point3D -> Point3D
move (dx, dy, dz) (x, y, z) = (x + dx, y + dy, z + dz)

frame :: Float -> Picture
frame seconds = pictures [coordSys 100.0 (0.0, 0.0, 400.0) (seconds * 35.0) (seconds * 45.0),
                          Color black $ line imageSpaceCoords]
                where offset = move (0.0, 0.0, 400.0)
                      rotate' = rotateX (seconds * 35.0) .
                                rotateZ (seconds * 45.0)
                      points = take 10000 $ lorenzPoints (0.01, 0.0, 0.0)
                      moved = map (offset . rotate') points
                      imageSpaceCoords = map project moved

main :: IO ()
main = animate window white frame
