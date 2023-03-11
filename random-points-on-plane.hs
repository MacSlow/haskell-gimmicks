-- because of ghc's/Haskell's type-conversion
-- weirdness better compile like this:
--
--   ghc -dynamic
--        -O2
--        -threaded
--        -Wall
--        -rtsopts
--        --make random-points-on-plane.hs -o random-points-on-plane
--

import System.IO ()
import System.Random
import Graphics.Gloss

dot :: Color -> Point -> Picture
dot col (x, y) = Color col $ translate x y $ thickCircle 1.25 2.5

modf :: Float -> Float -> Float
modf a b | a < b = a
         | a > b = a - b*integral
         | otherwise = 0.0
         where integral = fromIntegral $ fst $ properFraction $ a / b

hsvHelper :: Float -> Float -> Float -> (Float, Float, Float)
hsvHelper h c x
  | h >= 0.0   && h <  60.0 = (  c,   x, 0.0)
  | h >= 60.0  && h < 120.0 = (  x,   c, 0.0)
  | h >= 120.0 && h < 180.0 = (0.0,   c,   x)
  | h >= 180.0 && h < 240.0 = (0.0,   x,   c)
  | h >= 240.0 && h < 300.0 = (  x, 0.0,   c)
  | h >= 300.0 && h < 360.0 = (  c, 0.0,   x)
  | otherwise = (0.0, 0.0, 0.0)

-- helpers to deal with 3-component tuples
first :: Num n => (n, n, n) -> n
first (a, _, _) = a

second :: Num n => (n, n, n) -> n
second (_, b, _) = b

third :: Num n => (n, n, n) -> n
third (_, _, c) = c

-- colorFromHSV takes h s v as arguments
-- h - hue 0.0..360.0
-- s - saturation 0.0..100.0
-- v - value 0.0..100.0
colorFromHSV :: Float -> Float -> Float -> Color
colorFromHSV h s v = makeColor r g b a
  where c   = v/100.0 * s/100.0
        x   = c * (1.0 - abs ((h / 60.0) `modf` 2.0 - 1.0))
        m   = v/100.0 - c
        rgb = hsvHelper h c x
        r'  = first rgb
        g'  = second rgb
        b'  = third rgb
        r   = r' + m
        g   = g' + m
        b   = b' + m
        a   = 1.0

width :: Int
width = 1280

half_width :: Float
half_width = (fromRational $ toRational width) / 2.0

height :: Int
height = 720

half_height :: Float
half_height = (fromRational $ toRational height) / 2.0

window :: Display
window = InWindow "random points on plane with Haskell" (width, height) (20, 20)

points :: [Point]
points = zip x y
  where  x = take 1000 $ randomRs (-half_width, half_width) $ mkStdGen 1337
         y = take 1000 $ randomRs (-half_height, half_height) $ mkStdGen 4711

offset :: Float -> Float -> Point -> Point
offset angle magnitude point = (magnitude*offset_x + fst point, magnitude*offset_y + snd point)
  where alpha = pi / 180.0 * angle
        offset_x = cos alpha
        offset_y = sin alpha

lerp :: Float -> Float -> Float -> Float
lerp a b k
  | a <= b = a + abs (a - b)*k
  | a > b = b + abs (b - a)*k
  | otherwise = 0.0

noise :: Int -> Int -> (Float, Float) -> Float
noise width' height' (x, y) = result
  where octave_map = map (\seed -> (take width' $ randomRs (0.0::Float, 1.0) $ mkStdGen (1337 + seed*1000)) ) [1..height']
        column_lower = (fromIntegral $ floor x)::Int
        row_lower = (fromIntegral $ floor y)::Int
        column_upper = (fromIntegral $ ceiling x)::Int
        row_upper = (fromIntegral $ ceiling y)::Int
        a = (octave_map !! row_lower) !! column_lower
        b = (octave_map !! row_lower) !! column_upper
        c = (octave_map !! row_upper) !! column_lower
        d = (octave_map !! row_upper) !! column_upper
        lower_row_value = lerp a b (snd $ properFraction x)
        upper_row_value = lerp c d (snd $ properFraction x)
        result = lerp lower_row_value upper_row_value (snd $ properFraction y)

remap :: Float -> Float -> Float -> Float -> Float -> Float
remap old_start old_end new_start new_end value = result
  where a = (old_end - old_start)
        b = (new_end - new_start)
        c = (value - old_start)
        result = new_start + c*(b/a)

frame :: Float -> Picture
frame seconds = pictures (map (\(hue, point) -> dot (colorFromHSV hue 75.0 85.0) point) moved)
  where moved = map (\point -> (360.0 * (noise 50 50 ((remap_x $ fst point), (remap_y $ snd point))), (offset angle magnitude point))) points
        angle = (randomRs (0.0::Float, 360.0) $ mkStdGen seed) !! 1
        magnitude = 0.0 --(randomRs (0.1::Float, 5.0) $ mkStdGen seed) !! 2
        seed = floor (seconds*100)::Int
        remap_x = remap (-half_width) half_width 1.0 49.0
        remap_y = remap (-half_height) half_height 1.0 49.0

main :: IO ()
main = animate window (makeColor 0.1 0.1 0.1 1.0) frame
