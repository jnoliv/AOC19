import Data.List.Split (splitOn)

type Program = [Int]

toProgram :: String -> Program
toProgram = map read . splitOn ","

setAt :: Int -> a -> [a] -> [a]
setAt pos val l = take pos l ++ [val] ++ drop (pos + 1) l

getAt :: Int -> [a] -> a
getAt pos = head . drop pos

process :: Program -> Int
process l = processAux 0 l

processAux :: Int -> Program -> Int
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

main :: IO ()
main = do
    contents <- getContents

    let codes = setAt 1 12 . setAt 2 2 . toProgram $ contents
    print $ process codes
