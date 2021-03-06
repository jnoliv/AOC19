import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

data Point = P (Int, Int) Int -- (x,y) traveled_length
type PointSet = Set.Set Point

instance Eq Point where
    (P c1 _) == (P c2 _) = c1 == c2

instance Ord Point where
    compare (P c1 _) (P c2 _) = compare c1 c2

add :: Point -> Point -> Point
(P (x1,y1) l1) `add` (P (x2,y2) l2) = P (x1 + x2, y1 + y2) (l1 + l2)

mult :: Int -> Point -> Point
n `mult` (P (x,y) l) = P (n * x, n * y) (n * l)

manhattanDist :: Point -> Point -> Int
manhattanDist (P (x1,y1) _) (P (x2,y2) _) = (abs $ x1 - x2) + (abs $ y1 - y2)

steps :: Point -> Int
steps (P _ l) = l


toDir :: String -> (Char, Int)
toDir (dir:count) = (dir, read count)

pointsSet :: [(Char, Int)] -> PointSet
pointsSet points = pointsSet' (P (0,0) 0) points

pointsSet' :: Point -> [(Char, Int)] -> PointSet
pointsSet' curPoint [] = Set.empty
pointsSet' curPoint (dir:dirs) = 
    let len = snd dir
        pDir = case fst dir of
            'U' -> P (0, 1) 1
            'R' -> P (1, 0) 1
            'D' -> P (0, -1) 1
            'L' -> P (-1, 0) 1
        nextPoint = curPoint `add` (len `mult` pDir)
        newPoints = Set.fromList $ map (add curPoint) [n `mult` pDir | n <- [0..(len - 1)]]
    in Set.union newPoints $ pointsSet' nextPoint dirs

toIntersectionList :: PointSet -> PointSet -> PointSet -> [Point]
toIntersectionList set1 set2 intersec =
    let getPoint set point = fromMaybe (P (0,0) 0) $ Set.lookupLE point set 
        totalSteps (P c _) = P c $ steps (getPoint set1 (P c 0)) + steps (getPoint set2 (P c 0))
    in Set.toList $ Set.map totalSteps $ intersec

main :: IO ()
main = do
    contents <- getContents

    let ls = map (map toDir . splitOn ",") $ lines contents

    let [set1,set2] = map pointsSet ls
    let intersections = Set.delete (P (0,0) 0) $ Set.intersection set1 set2

    let output1 = minimum . map (manhattanDist (P (0,0) 0)) $ Set.toList intersections

    let intList = toIntersectionList set1 set2 $ intersections
    let output2 = minimum $ map steps intList

    putStrLn $ "Part1: " ++ show output1
    putStrLn $ "Part2: " ++ show output2
