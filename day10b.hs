import Data.Fixed
import Data.List
import Data.Maybe

type Vector = (Int, Int)
data NormVector = NormVector Int (Int,Int) Float

instance Show NormVector where
    show (NormVector norm (x,y) a) =
        show norm ++ "(" ++ show x ++ "," ++ show y ++ ")[" ++ show a ++ "]"

nvOrd :: NormVector -> NormVector -> Ordering
nvOrd (NormVector n1 _ a1) (NormVector n2 _ a2) =
    if a1 > a2 then GT else
        if a1 < a2 then LT else
            if n1 > n2 then GT else
                if n1 < n2 then LT else EQ

main = do
    contents <- getContents
    let astL = toAstList $ words contents
    let counts = map (countUnobstructed astL) $ astL
    print . maximum $ counts

    let indexBest = fromMaybe 0 $ elemIndex (maximum counts) counts
    print (astL !! indexBest)
    let fromBest = changeReferential (astL !! indexBest) astL
    let sorted = sortBy nvOrd $ map normalizeVector $ fromBest
    print sorted
    
    let (NormVector n (x,y) _) = getNthVaped 200 1 $ tail sorted
    let (x2,y2) = resetReferential (n*x,n*y) (astL !! indexBest)
    print $ x2 * 100 + y2

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

resetReferential :: Vector -> Vector -> Vector
resetReferential (x,y) (curX,curY) = (x + curX, y + curY)

normalizeVector :: Vector -> NormVector
normalizeVector (i,j) =
    let gcdNorm = gcd i j
    in if gcdNorm == 0
        then NormVector 0 (0,0) 0
        else let 
            normedI = i `div` gcdNorm
            normedJ = j `div` gcdNorm
            floatI = fromIntegral normedI :: Float
            floatJ = fromIntegral normedJ :: Float
            norm = sqrt ((floatI^2) + (floatJ^2))
            angle = acos $ (-floatJ) / norm
            correctedAngle = if i < 0 then 2 * pi - angle else angle
        in NormVector gcdNorm (normedI, normedJ) correctedAngle

removeObstructed :: [NormVector] -> [NormVector] -> [NormVector]
removeObstructed [] out = out
removeObstructed (l:ls) out = 
    let NormVector norm (x,y) a = l
    in removeObstructed ls $ case (findNV l out) of
        Nothing -> out ++ [l]
        Just (NormVector norm2 _ _) -> 
            if norm2 < norm
                then out
                else replace (NormVector norm (x,y) a) out

findNV :: NormVector -> [NormVector] -> Maybe NormVector
findNV _ [] = Nothing
findNV (NormVector norm (x,y) a) (NormVector norm2 (x2,y2) a2 : ls) =
    if (x == x2) && (y == y2)
        then Just (NormVector norm2 (x2,y2) a2)
        else findNV (NormVector norm (x,y) a) ls

replace :: NormVector -> [NormVector] -> [NormVector]
replace v [] = [v]
replace (NormVector norm (x,y) a) (NormVector norm2 (x2,y2) a2 : ls) =
    if (x == x2) && (y == y2)
        then (NormVector norm (x,y) a) : ls
        else (NormVector norm2 (x2,y2) a2) : replace (NormVector norm (x,y) a) ls

countUnobstructed :: [Vector] -> Vector -> Int
countUnobstructed l v = 
    let normalized = map normalizeVector $ changeReferential v l
        len = length $ removeObstructed normalized []
    in len - 1 -- disregard self

getNthVaped :: Int -> Int -> [NormVector] -> NormVector
getNthVaped n curN (x:xs) = 
    if n == curN
        then x
        else
            let NormVector _ _ a = x 
                nSame = countSame a xs
            in getNthVaped n (curN + 1) $ drop nSame xs ++ take nSame xs

countSame :: Float -> [NormVector] -> Int
countSame _ [] = 0
countSame n (NormVector _ _ a : xs) =
    if n /= a
        then 0
        else 1 + countSame n xs
