import Data.List.Split (splitOn)
import qualified Data.Set as Set

type Point = (Int, Int)
type PointSet = Set.Set Point

add :: Point -> Point -> Point
(x1,y1) `add` (x2,y2) = (x1 + x2, y1 + y2)

mult :: Int -> Point -> Point
n `mult` (x,y) = (n * x, n * y)

dist :: Point -> Point -> Int
dist (x1,y1) (x2,y2) = (abs $ x1 - x2) + (abs $ y1 - y2)


toDir :: String -> (Char, Int)
toDir (dir:count) = (dir, read count)

pointsSet :: [(Char, Int)] -> PointSet
pointsSet points = pointsSet' (0,0) points

pointsSet' :: Point -> [(Char, Int)] -> PointSet
pointsSet' curPoint [] = Set.empty
pointsSet' curPoint (dir:dirs) = 
    let len = snd dir
        pDir = case fst dir of
            'U' -> (0, 1)
            'R' -> (1, 0)
            'D' -> (0, -1)
            'L' -> (-1, 0)
        nextPoint = curPoint `add` (len `mult` pDir)
        newPoints = Set.fromList $ map (add curPoint) [n `mult` pDir | n <- [0..(len - 1)]]
    in Set.union newPoints $ pointsSet' nextPoint dirs

main :: IO ()
main = do
    contents <- getContents

    let [l1,l2] = map (map toDir . splitOn ",") $ lines contents

    let ints = Set.toList . Set.delete (0,0) $ Set.intersection (pointsSet l1) (pointsSet l2)
    let minDist = minimum $ map (dist (0,0)) ints

    print minDist
