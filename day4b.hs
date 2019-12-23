import Data.Char (digitToInt)
import Data.List (sort, group)
import Data.List.Split (splitOn)

main = do
    contents <- getContents

    let range = map (map digitToInt) . splitOn "-" $ contents

    print $ countPossiblePasses range

countPossiblePasses :: [[Int]] -> Int
countPossiblePasses [b,e] = countPossiblePasses' b e 0 []

countPossiblePasses' :: [Int] -> [Int] -> Int -> [Int] -> Int
countPossiblePasses' beg end 6 cur = 
    if hasExactlyTwoAdjacentEqual cur then 1 else 0

countPossiblePasses' beg end ind cur = 
    if isNotInRange beg end cur
        then 0
        else
            let applied = countPossiblePasses' beg end (ind + 1)
                lastChar = if length cur == 0 then 0 else (last cur)
            in sum . map applied $ [cur ++ [c] | c <- [lastChar..9]]

isNotInRange :: [Int] -> [Int] -> [Int] -> Bool
isNotInRange b e c = not $ (c `isGreaterOrEqual` b) && (c `isLessOrEqual` e)

isGreaterOrEqual :: [Int] -> [Int] -> Bool
isGreaterOrEqual [] y = True
isGreaterOrEqual (x:xs) (y:ys) =
    if x < y then False else
        if x > y then True else isGreaterOrEqual xs ys

isLessOrEqual :: [Int] -> [Int] -> Bool
isLessOrEqual [] y = True
isLessOrEqual (x:xs) (y:ys) =
    if x > y then False else
        if x < y then True else isLessOrEqual xs ys

hasTwoAdjacentEqual :: [Int] -> Bool
hasTwoAdjacentEqual (x:[]) = False
hasTwoAdjacentEqual (x:xs) = 
    if x == (head xs) then True else hasTwoAdjacentEqual xs

hasExactlyTwoAdjacentEqual :: [Int] -> Bool
hasExactlyTwoAdjacentEqual l =
    any (== 2) $ map length . group . sort $ l
