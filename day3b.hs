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

dist :: Point -> Int
dist (P _ l) = l


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
        totalSteps (P c _) = P c $ dist (getPoint set1 (P c 0)) + dist (getPoint set2 (P c 0))
    in Set.toList $ Set.map totalSteps $ Set.delete (P (0,0) 0) intersec

main :: IO ()
main = do
    contents <- getContents

    let ls = map (map toDir . splitOn ",") $ lines contents

    let [set1,set2] = map pointsSet ls
    let ints = toIntersectionList set1 set2 $ Set.intersection set1 set2
    let minDist = minimum $ map dist ints

    print minDist
