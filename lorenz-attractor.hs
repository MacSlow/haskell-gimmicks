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
