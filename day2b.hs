import Data.List (find)
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

tryAll :: Program -> Int -> (Int, Int)
tryAll prog val =
    dropLast $ find (\(_,_,v) -> val == v) [(n, v, process' n v) | n <- [0..99], v <- [0..99]]
    where
        process' n v = process . setAt 1 n . setAt 2 v $ prog
        dropLast (Just (n,v,_)) = (n,v)
        dropLast Nothing = (-1, -1)

main:: IO ()
main = do
    contents <- getContents

    let codes = toProgram contents
    let (noun, verb) = tryAll codes 19690720

    print $ 100 * noun + verb
