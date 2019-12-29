import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

type Vector = (Int, Int) -- (y,x)
data NormVector = NormVector Int Vector Float deriving (Show)

instance Eq NormVector where
    (NormVector _ _ a1) == (NormVector _ _ a2) = a1 == a2

instance Ord NormVector where
    (NormVector n1 _ a1) <= (NormVector n2 _ a2) =
        if a1 == a2
            then n1 <= n2
            else a1 <= a2

negateVector :: Vector -> Vector
negateVector (y,x) = (-y, -x)

changeReferential :: Vector -> [Vector] -> [Vector]
changeReferential (newY, newX) vectors =
    map (\(y,x) -> (y - newY, x - newX)) vectors

normalizeVector :: Vector -> NormVector
normalizeVector (y,x) =
    let gcdNorm = gcd y x
        normedX = x `div` gcdNorm
        normedY = y `div` gcdNorm
        floatY = fromIntegral normedY :: Float
        norm = sqrt $ (fromIntegral normedX ^ 2) + (floatY ^ 2)
        angle = acos $ (-floatY) / norm
        correctedAngle = if x < 0 then 2 * pi - angle else angle
    in if gcdNorm == 0
        then NormVector 0 (0,0) 0
        else NormVector gcdNorm (normedY, normedX) correctedAngle

deNormalizeVector :: NormVector -> Vector
deNormalizeVector (NormVector norm (y,x) _) = (y * norm, x * norm)

countUnobstructed :: [Vector] -> Vector -> Int
countUnobstructed l v = 
    let normalized = map normalizeVector $ changeReferential v l
        normalized' = tail . sort $ normalized -- remove current ast
    in Set.size $ foldr Set.insert Set.empty normalized'

toAstList :: [String] -> [Vector]
toAstList lines =
    let maxRow = length lines - 1
        maxCol = length (head lines) - 1
    in [(y,x) | y <- [0..maxRow], x <- [0..maxCol], lines !! y !! x == '#']

sortByVaped :: Vector -> [Vector] -> [Vector]
sortByVaped origin astL =
    let astL' = map normalizeVector $ changeReferential origin astL
        sorted = sortByVaped' . tail . sort $ astL'
        deNormed = map deNormalizeVector sorted
    in changeReferential (negateVector origin) deNormed
    where
        sortByVaped' [] = []
        sortByVaped' (a:as) =
            let nSame = length $ takeWhile (== a) as
            in a : sortByVaped' (drop nSame as ++ take nSame as)

main :: IO ()
main = do
    contents <- getContents

    let astL = toAstList $ lines contents
    let unobstructedCounts = map (countUnobstructed astL) astL
    let maxUnobstructed = maximum unobstructedCounts
    
    putStrLn $ "Part1: " ++ show maxUnobstructed

    let bestAstIndex = fromJust $ maxUnobstructed `elemIndex` unobstructedCounts
    let sortedByVapeOrder = sortByVaped (astL !! bestAstIndex) astL
    let (y,x) = sortedByVapeOrder !! 199 -- 200th to be vaped is at position 199

    putStrLn $ "Part2: " ++ show (x * 100 + y)
