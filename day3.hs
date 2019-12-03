import qualified Data.Set as S

main = do
    contents <- getContents
    let ls = lines contents
    let l1 = map toDir . split $ (head ls)
    let l2 = map toDir . split $ (head (drop 1 ls))
    let ints = S.toList $ S.intersection (pointsSet l1) (pointsSet l2)
    let dists = map distance ints
    print (minimum dists)


split :: String -> [String]
split l = splitAux ',' l []

splitAux :: Char -> String -> String -> [String]
splitAux del [] cur = [cur]
splitAux del (x:xs) cur =
    if x == del
        then cur : splitAux del xs []
        else splitAux del xs (cur ++ [x])

toDir :: String -> (Char, Int)
toDir (dir:count) = (dir, read count)

pointsSet :: [(Char, Int)] -> S.Set (Int, Int)
pointsSet points = pointsSetAux (0,0) points S.empty


pointsSetAux :: (Int, Int) -> [(Char, Int)] -> S.Set (Int, Int) -> S.Set (Int, Int)
pointsSetAux curPoint [] res = res
pointsSetAux curPoint (point:points) res = 
    let len = snd point
        dir = case fst point of
            'U' -> (0, 1)
            'R' -> (1, 0)
            'D' -> (0, -1)
            'L' -> (-1, 0)
        nextPoint = addToPoint curPoint $ multPoint dir len
        nextSet = addPoints curPoint dir len res
    in pointsSetAux nextPoint points nextSet

multPoint :: (Int, Int) -> Int -> (Int, Int)
multPoint (x, y) n = (x * n, y * n)

addPoints :: (Int, Int) -> (Int, Int) -> Int -> S.Set (Int, Int) -> S.Set (Int, Int)
addPoints curPoint dir 0 set = set
addPoints curPoint dir count set = 
    let newPoint = addToPoint curPoint (multPoint dir count)
    in addPoints curPoint dir (count - 1) (S.insert newPoint set)

addToPoint :: (Int, Int) -> (Int, Int) -> (Int, Int)
addToPoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

distance :: (Int, Int) -> Int
distance (x, y) = (abs x) + (abs y)
