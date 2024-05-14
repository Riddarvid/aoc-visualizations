module AoCVisualizations.V2324 (displayDay24) where
import           Data.Bifunctor (Bifunctor (bimap))
import           Days.Day24     (Hail (Hail), parseInput)
import           GHC.Float      (double2Float)
import           Graphics.Gloss (Display (..), Picture, color, display, greyN,
                                 line, pictures, rectangleSolid, red, scale,
                                 translate)
import           Linear         (Additive ((^+^)), V3 (V3), (*^))

displayDay24 :: String -> IO ()
displayDay24 input = display (InWindow "Day 24" (1000, 1000) (200, 200)) (greyN 0.9)
  (renderGraph hails)
  where
    hails = parseInput input

renderGraph :: [Hail] -> Picture
renderGraph hails = scaled
  where
    basePic = pictures $ map (renderHail 0 1E16) hails
    --basePic = pictures $ map (renderHailPos 2E12) hails
    withAxes = basePic <> color red (renderAxes 2E14 2E14)
    translated = translate (-2E14) (-2E14) withAxes
    scaled = scale 1E-10 1E-10 translated

renderAxes :: Float -> Float -> Picture
renderAxes x y = xAxis <> yAxis
  where
    xAxis = line [(x, -inf), (x, inf)]
    yAxis = line [(-inf, y), (inf, y)]
    inf = 1E20

renderHail :: Float -> Float -> Hail -> Picture
renderHail t1 t2 (Hail p v) = line [(x1, y1), (x2, y2)]
  where
    (V3 x1 y1 _) = (fromInteger <$> p) ^+^ (t1 *^ (fromInteger <$> v))
    (V3 x2 y2 _) = (fromInteger <$> p) ^+^ (t2 *^ (fromInteger <$> v))

renderHailPos :: Float -> Hail -> Picture
renderHailPos t (Hail p v) = pic
  where
    (V3 x y _) = (fromInteger <$> p) ^+^ (t *^ (fromInteger <$> v))
    base = rectangleSolid (1E12 * 5) (1E12 * 5)
    pic = translate x y base
