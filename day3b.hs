type MyPoint = (Int, Int, Int)

main = do
    contents <- getContents
    let ls = lines contents
    let l1 = map toDir . split $ (head ls)
    let l2 = map toDir . split $ (head (drop 1 ls))
    let ints = mySetIntersection (pointsSet l1) (pointsSet l2)
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

pointsSet :: [(Char, Int)] -> [MyPoint]
pointsSet points = pointsSetAux (0,0,0) points []


pointsSetAux :: MyPoint -> [(Char, Int)] -> [MyPoint] -> [MyPoint]
pointsSetAux curPoint [] res = res
pointsSetAux curPoint (point:points) res = 
    let len = snd point
        dir = case fst point of
            'U' -> (0, 1, 1)
            'R' -> (1, 0, 1)
            'D' -> (0, -1, 1)
            'L' -> (-1, 0, 1)
        nextPoint = addToPoint curPoint $ multPoint dir len
        nextSet = addPoints curPoint dir len res
    in pointsSetAux nextPoint points nextSet

multPoint :: MyPoint -> Int -> MyPoint
multPoint (x, y, s) n = (x * n, y * n, s * n)

addPoints :: MyPoint -> MyPoint -> Int -> [MyPoint] -> [MyPoint]
addPoints curPoint dir 0 set = set
addPoints curPoint dir count set = 
    let newPoint = addToPoint curPoint (multPoint dir count)
    in addPoints curPoint dir (count - 1) (mySetInsert set newPoint)

addToPoint :: MyPoint -> MyPoint -> MyPoint
addToPoint (x1, y1, s1) (x2, y2, s2) = (x1 + x2, y1 + y2, s1 + s2)

distance :: MyPoint -> Int
distance (x, y, s) = s

mySetInsert :: [MyPoint] -> MyPoint -> [MyPoint]
mySetInsert set point =
    case mySetFind set point of
        Just point -> set
        Nothing -> set ++ [point]

mySetFind :: [MyPoint] -> MyPoint -> Maybe MyPoint
mySetFind [] point = Nothing
mySetFind (p:ps) (x1,y1,s1) =
    let (x2,y2,s2) = p
    in if (x1 == x2) && (y1 == y2)
        then Just p
        else mySetFind ps (x1,y1,s1)

mySetIntersection :: [MyPoint] -> [MyPoint] -> [MyPoint]
mySetIntersection s1 s2 = mySetIntersectionAux s1 s2 []

mySetIntersectionAux :: [MyPoint] -> [MyPoint] -> [MyPoint] -> [MyPoint]
mySetIntersectionAux [] s2 res = res
mySetIntersectionAux (p:s1s) set2 res =
    let (x1,y1,s1) = p
        pointS2 = mySetFind set2 p
    in case pointS2 of
        Just (x2,y2,s2) -> mySetIntersectionAux s1s set2 (res ++ [(x1,y1, s1 + s2)])
        Nothing -> mySetIntersectionAux s1s set2 res
