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
project (x0, y0, z0) = (projectedX, projectedY)
    where (lookAtX, lookAtY, lookAtZ) = (0.0, 0.0, 0.0)
          (x, y, z) = (x0 - lookAtX, y0 - lookAtY, z0 - lookAtZ)
          (alpha, beta, gamma) = (degToRad 0.0, degToRad 0.0, degToRad 0.0)
          (eyeX, eyeY, eyeZ) = (0.0, 0.0, 300.0)
          (cosAlpha, sinAlpha) = (cos alpha, sin alpha)
          (cosBeta, sinBeta) = (cos beta, sin beta)
          (cosGamma, sinGamma) = (cos gamma, sin gamma)
          (dx, dy, dz) = (cosBeta*(sinGamma*y + cosGamma*x) - sinBeta*z,
                          sinAlpha*(cosBeta*z + sinBeta*(sinGamma*y + cosGamma*x)) +
                          cosAlpha*(cosGamma*y - sinGamma*x),
                          cosAlpha*(cosBeta*z + sinBeta*(sinGamma*y + cosGamma*x)) -
                          sinAlpha*(cosGamma*y - sinGamma*x)) 
          projectedX = eyeZ/dz*dx - eyeX
          projectedY = eyeZ/dz*dy - eyeY

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
