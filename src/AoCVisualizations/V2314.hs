module AoCVisualizations.V2314 (animateDay14) where
import           AoCUtils.Geometry (Point2 (P2))
import           AoCVisualizations.Utils (renderSquare, tick)
import           AoCUtils.Matrices (matrixToHashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS
import           Days.Day14        (Pos, generateMoves)
import           Graphics.Gloss    (Display (InWindow), Picture, animate, black,
                                    greyN, pictures, red, white)

animateDay14 :: String -> IO ()
animateDay14 input =
  animate (InWindow "Day 14" (200, 200) (10, 10)) (greyN 0.9) (renderTilts input)

renderTilts :: String -> Float -> Picture
renderTilts input = tick 0.1 pics
  where
    (rocks, maxX, maxY) = matrixToHashMap $ lines input
    cubes = HM.keysSet $ HM.filter (== '#') rocks
    rounds = HM.keysSet $ HM.filter (== 'O') rocks
    pics = take 1000 $ map (renderRocks maxX maxY cubes) $ generateMoves maxX maxY cubes rounds

renderRocks :: Int -> Int -> HashSet Pos -> HashSet Pos -> Picture
renderRocks maxX maxY cubes rounds = pictures $
  [renderRock cubes rounds (P2 x y) | x <- [0 .. maxX], y <- [0 .. maxY]]

renderRock :: HashSet Pos -> HashSet Pos -> Pos -> Picture
renderRock cubes rounds p = renderSquare color' p
  where
    color'
      | p `HS.member` cubes = black
      | p `HS.member` rounds = red
      | otherwise = white
