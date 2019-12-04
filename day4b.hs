import Data.Char
import Data.List

main = do
    contents <- getContents
    let range = map (map digitToInt) . split $ contents
    print (countPossiblePasses range)

split :: String -> [String]
split l = splitAux '-' l []

splitAux :: Char -> String -> String -> [String]
splitAux del [] cur = [cur]
splitAux del (x:xs) cur =
    if x == del
        then cur : splitAux del xs []
        else splitAux del xs (cur ++ [x])

countPossiblePasses :: [[Int]] -> Int
countPossiblePasses (b:e) =
    countPossiblePassesAux b (head e) 0 0 []

countPossiblePassesAux :: [Int] -> [Int] -> Int -> Int -> [Int] -> Int
countPossiblePassesAux beg end 6 count cur = 
    if (hasExactlyTwoAdjacentEqual cur) then 1 else 0

countPossiblePassesAux beg end ind count cur = 
    if not (isInRange beg end cur) then 0
        else let applied = countPossiblePassesAux beg end (ind + 1) count
        in sumPossibleChildPasses applied cur (
            if (length cur) == 0 then 0 else (last cur))

sumPossibleChildPasses :: ([Int] -> Int) -> [Int] -> Int -> Int
sumPossibleChildPasses f cur 10 = 0
sumPossibleChildPasses f cur ind =
    (f (cur ++ [ind])) + (sumPossibleChildPasses f cur (ind + 1))


isInRange :: [Int] -> [Int] -> [Int] -> Bool
isInRange b e c = (c `isGreaterOrEqual` b) && (c `isLessOrEqual` e)

isGreaterOrEqual :: [Int] -> [Int] -> Bool
isGreaterOrEqual [] y = True
isGreaterOrEqual (x:xs) (y:ys) =
    if x < y
        then False
        else if x > y
            then True
            else isGreaterOrEqual xs ys

isLessOrEqual :: [Int] -> [Int] -> Bool
isLessOrEqual [] y = True
isLessOrEqual (x:xs) (y:ys) =
    if x > y
        then False
        else if x < y
            then True
            else isLessOrEqual xs ys

hasExactlyTwoAdjacentEqual :: [Int] -> Bool
hasExactlyTwoAdjacentEqual l =
    any (\x -> x == 2) $ map (\x -> length x) . group . sort $ l
