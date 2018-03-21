import Graphics.Gloss
import System.IO ()

bigDot :: Color -> Point -> Picture
bigDot col (x, y) = Color col $ translate x y $ thickCircle 1.0 2.0

redDot :: Point -> Picture
redDot (x, y) = bigDot red (x, y)

blackLine :: Path -> Picture
blackLine path = Color black $ line path

blueLine :: Path -> Picture
blueLine path = Color blue $ line path

controlLine :: Path -> Picture
controlLine path = pictures [blueLine path, redDot a, redDot b]
    where a = head path
          b = head $ tail path

type PointGenerator = (Float -> Point -> Point -> Point -> Point -> Point)

curve :: PointGenerator -> Int -> Point -> Point -> Point -> Point -> Path
curve pg res cp0 cp1 cp2 cp3 = map (\t -> pg t cp0 cp1 cp2 cp3) $
                               take (res + 1) $ iterate (+ step) 0.0
                               where step = 1.0 / fromIntegral res::Float

cubic :: PointGenerator
cubic t cp0 cp1 cp2 cp3 = (xnew, ynew)
   where (x0, y0) = cp0
         (x1, y1) = cp1 
         (x2, y2) = cp2
         (x3, y3) = cp3 
         a = 1.0 - t
         xnew = a^(3::Int)*x0 +
                3.0*a^(2::Int)*t*x1 +
                3.0*a*t^(2::Int)*x2 +
                t^(3::Int)*x3
         ynew = a^(3::Int)*y0 +
                3.0*a^(2::Int)*t*y1 +
                3.0*a*t^(2::Int)*y2 +
                t^(3::Int)*y3

middle :: Point -> Point -> Point
middle (x0, y0) (x1, y1) = ((x0 + x1)/2.0, (y0 + y1)/2.0)

window :: Display
window = (InWindow "Cubic Bézier-Curves in Haskell" (600, 450) (20, 20))

frame :: Float -> Picture
frame timeInSeconds = pictures [blackLine path0,
                                blackLine path1,
                                controlLine [p0, p1],
                                controlLine [p2, p6],
                                controlLine [p6, p3],
                                controlLine [p4, p5]]
                                where c = cos timeInSeconds
                                      s = sin timeInSeconds
                                      p0 = (-120.0 * s, -100.0 * c)
                                      p1 = (70.0 * s, 150.0 * s)
                                      p2 = (90.0 * c, -160.0 * s)
                                      p3 = (-90.0 * c, 160.0 * s)
                                      p4 = (100.0 * c, -100.0 * s)
                                      p5 = (-150.0 * c, 150.0 * c)
                                      p6 = middle p2 p3
                                      path0 = curve cubic 50 p0 p1 p2 p6
                                      path1 = curve cubic 50 p6 p3 p4 p5

main :: IO ()
main = animate window white frame 

