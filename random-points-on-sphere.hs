import System.IO ()
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

window :: Display
window = InWindow "random points on sphere with Haskell" (600, 450) (20, 20)

dot :: Color -> Point -> Picture
dot col (x, y) = Color col $ translate x y $ thickCircle 1.0 2.0

redDot :: Point -> Picture
redDot (x, y) = dot red (x, y)

type Point3D = (Float, Float, Float)

polarToCartesian :: Float -> Float -> Float -> Point3D
polarToCartesian theta gamma radius = (x, y, z)
    where thetaRad = degToRad theta
          gammaRad = degToRad gamma
          sinTheta = sin thetaRad
          x = radius * sinTheta * cos gammaRad
          y = radius * sinTheta * sin gammaRad
          z = radius * cos thetaRad

sphere :: [Point3D]
sphere = map (\(t, g) -> polarToCartesian t g 250.0) randomAnglePairs
    where randomAnglePairs = zip thetas gammas
          thetas = take 750 $ randomRs (0.0::Float, 360.0) $ mkStdGen 42
          gammas = take 750 $ randomRs (0.0::Float, 360.0) $ mkStdGen 1337

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

frame :: Float -> Picture
frame seconds = pictures (map redDot imageSpaceCoords)
                where offset = move (0.0, 0.0, 400.0)
                      rotate' = rotateX (seconds * 35.0) .
                                rotateZ (seconds * 45.0)
                      moved = map (offset . rotate') sphere
                      imageSpaceCoords = map project moved

main :: IO ()
main = animate window white frame
