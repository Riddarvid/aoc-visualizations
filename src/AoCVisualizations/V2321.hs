module AoCVisualizations.V2321 (displayDay21) where
import           AoCUtils.Geometry (Point2 (P2))
import           AoCVisualizations.Utils (renderSquare)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS
import           Data.Maybe        (fromJust)
import           Days.Day21        (Tile (..), solveGraphics)
import           Graphics.Gloss    (Display (InWindow), Picture, black, display,
                                    greyN, pictures, red, white)

type Pos = Point2 Int

displayDay21 :: String -> IO ()
displayDay21 input = display (InWindow "Day 21" (1000, 1000) (200, 200)) (greyN 0.9)
  (renderMap maxX maxY tiles exactReachable)
  where
    (maxX, maxY, tiles, exactReachable) = solveGraphics input

renderMap :: Int -> Int -> HashMap Pos Tile -> HashSet Pos -> Picture
renderMap maxX maxY tiles exactReachable = pictures $
  [renderPos tiles exactReachable (P2 x y) | x <- [0 .. maxX], y <- [0 .. maxY]]

renderPos :: HashMap Pos Tile -> HashSet Pos -> Pos -> Picture
renderPos tiles exactReachable pos = renderSquare color' pos
  where
    color'
      | HS.member pos exactReachable = red
      | otherwise = case fromJust (HM.lookup pos tiles) of
          Plot -> white
          Rock -> black
