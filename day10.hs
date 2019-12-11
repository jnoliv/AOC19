type Vector = (Int, Int)
data NormVector = NormVector Int (Int,Int)

instance Show NormVector where
    show (NormVector norm (x,y)) = show norm ++ "(" ++ show x ++ "," ++ show y ++ ")"

main = do
    contents <- getContents
    let astL = toAstList $ words contents
    print . maximum . map (countUnobstructed astL) $ astL

toAstList :: [String] -> [Vector]
toAstList lines = toAstListAux 0 lines

toAstListAux :: Int -> [String] -> [Vector]
toAstListAux _ [] = []
toAstListAux i (line:lines) = (parseLine 0 i line) ++ (toAstListAux (i + 1) lines)

parseLine :: Int -> Int -> String -> [Vector]
parseLine _ _ [] = []
parseLine i j (a:as) = case a of
    '#' -> (i,j):(parseLine (i + 1) j as)
    '.' -> parseLine (i + 1) j as

changeReferential :: Vector -> [Vector] -> [Vector]
changeReferential (newX, newY) vectors =
    map (\(x,y) -> (x - newX, y - newY)) vectors

normalizeVector :: Vector -> NormVector
normalizeVector (i,j) =
    let norm = gcd i j 
    in if norm == 0
        then NormVector 0 (0,0)
        else NormVector norm (i `div` norm, j `div` norm)

removeObstructed :: [NormVector] -> [NormVector] -> [NormVector]
removeObstructed [] out = out
removeObstructed (l:ls) out = 
    let NormVector norm (x,y) = l
    in removeObstructed ls $ case (find l out) of
        Nothing -> out ++ [l]
        Just (NormVector norm2 _) -> 
            if norm2 < norm
                then out
                else replace (NormVector norm (x,y)) out

find :: NormVector -> [NormVector] -> Maybe NormVector
find _ [] = Nothing
find (NormVector norm (x,y)) (NormVector norm2 (x2,y2) : ls) =
    --let areClose a b = (abs (a - b)) < 0.000000000000000000000000000000001
    --in if (areClose x x2) && (areClose y y2)
    if (x == x2) && (y == y2)
        then Just (NormVector norm2 (x2,y2))
        else find (NormVector norm (x,y)) ls

replace :: NormVector -> [NormVector] -> [NormVector]
replace v [] = [v]
replace (NormVector norm (x,y)) (NormVector norm2 (x2,y2) : ls) =
    if (x == x2) && (y == y2)
        then (NormVector norm (x,y)) : ls
        else (NormVector norm2 (x2,y2)) : replace (NormVector norm (x,y)) ls

countUnobstructed :: [Vector] -> Vector -> Int
countUnobstructed l v = 
    let normalized = map normalizeVector $ changeReferential v l
        len = length $ removeObstructed normalized []
    in len - 1 -- disregard self
