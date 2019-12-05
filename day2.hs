main = do
    contents <- getContents
    let codes = map (read :: String -> Int) . split $ contents
    let alteredCodes = setAt 2 2 (setAt 1 12 codes)
    print . process $ alteredCodes

split :: String -> [String]
split l = splitAux ',' l []

splitAux :: Char -> String -> String -> [String]
splitAux del [] cur = [cur]
splitAux del (x:xs) cur =
    if x == del
        then cur : splitAux del xs []
        else splitAux del xs (cur ++ [x])

setAt :: Int -> a -> [a] -> [a]
setAt pos val l = (take pos l) ++ (val : drop (pos + 1) l)

getAt :: Int -> [a] -> a
getAt 0 l = head l
getAt n l = head (drop n l)

process :: [Int] -> Int
process l = processAux 0 l

processAux :: Int -> [Int] -> Int
processAux ind l =
    let opcode = getAt ind l
        in1 = getAt (getAt (ind + 1) l) l
        in2 = getAt (getAt (ind + 2) l) l
        out = getAt (ind + 3) l
    in
        case opcode of
            1 -> processAux (ind + 4) $ setAt out (in1 + in2) l
            2 -> processAux (ind + 4) $ setAt out (in1 * in2) l
            99 -> head l