import Data.List

main = do
    contents <- getContents
    let codes = map (read :: String -> Int) . split $ contents
    let output = process codes [1]
    mapM print output

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

process :: [Int] -> [Int] -> [Int]
process l input = processAux 0 input [] l

processAux :: Int -> [Int] -> [Int] -> [Int] -> [Int]
processAux ind inL outL prog =
    let paramsOpcode = prog !! ind
        opcode = paramsOpcode `rem` 100
        pMode1 = (paramsOpcode `div` 100) `rem` 10
        pMode2 = (paramsOpcode `div` 1000) `rem` 10
        --pMode3 = (paramsOpcode `div` 10000) `rem` 10
    in case opcode of
        1 -> doAlgOp (+) ind inL outL prog pMode1 pMode2
        2 -> doAlgOp (*) ind inL outL prog pMode1 pMode2
        3 -> let param1 = prog !! (ind + 1)
                 value = head inL
            in processAux (ind + 2) (tail inL) outL $ setAt param1 value prog 
        4 -> let param1 = prog !! (ind + 1)
                 newOutL = outL ++ [prog !! param1]
            in processAux (ind + 2) inL newOutL prog
        99 -> outL
    where
        doAlgOp algOp ind inL outL prog pMode1 pMode2 =
            let in1 = prog !! (ind + 1)
                in2 = prog !! (ind + 2)
                in3 = prog !! (ind + 3)
                param1 = if pMode1 == 1 then in1 else (prog !! in1)
                param2 = if pMode2 == 1 then in2 else (prog !! in2)
            in processAux (ind + 4) inL outL $ setAt in3 (algOp param1 param2) prog
