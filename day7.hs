import Data.List
import Data.List.Split

main = do
    contents <- getContents
    let program = map read . splitOn "," $ contents
    print . maximum . map (getThrusterSignal program 0) $ permutations [0,1,2,3,4]

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
        --pMode3 = (paramsOpcode `div` 10000) `rem` 10 --pMode3 is always 0
    in case opcode of
        -- add first two params and store in third
        1 -> doAlgOp (+) ind inL outL prog pMode1 pMode2
        -- multiply first two params and store in third
        2 -> doAlgOp (*) ind inL outL prog pMode1 pMode2
        -- read one int from input and store in first param
        3 -> let param1 = prog !! (ind + 1)
                 value = head inL
            in processAux (ind + 2) (tail inL) outL $ setAt param1 value prog 
        -- read from first param and output it
        4 -> let param1 = prog !! (ind + 1)
                 newOutL = outL ++ [prog !! param1]
            in processAux (ind + 2) inL newOutL prog
        -- if first param != 0, set ind to second param
        5 -> doJmpOp ((/=) 0) ind inL outL prog pMode1 pMode2
        -- if first param == 0, set ind to second param
        6 -> doJmpOp ((==) 0) ind inL outL prog pMode1 pMode2
        -- if first param < second param, store 1 in third param, else store 0
        7 -> doCmpOp (<) ind inL outL prog pMode1 pMode2
        -- if first param == second param, store 1 in third param, else store 0
        8 -> doCmpOp (==) ind inL outL prog pMode1 pMode2
        -- halt execution
        99 -> outL
    where
        doAlgOp algOp ind inL outL prog pMode1 pMode2 =
            let (param1, param2) = getParams prog ind pMode1 pMode2
                param3 = prog !! (ind + 3)
            in processAux (ind + 4) inL outL $ setAt param3 (algOp param1 param2) prog
        doJmpOp jmpOp ind inL outL prog pMode1 pMode2 =
            let (param1, param2) = getParams prog ind pMode1 pMode2
                nextInd = if jmpOp param1 then param2 else (ind + 3)
            in processAux nextInd inL outL prog
        doCmpOp cmpOp ind inL outL prog pMode1 pMode2 =
            let (param1, param2) = getParams prog ind pMode1 pMode2
                param3 = prog !! (ind + 3)
                value = if (param1 `cmpOp` param2) then 1 else 0
            in processAux (ind + 4) inL outL $ setAt param3 value prog

getParams :: [Int] -> Int -> Int -> Int -> (Int, Int)
getParams prog ind pMode1 pMode2 =
    (getParam prog (ind + 1) pMode1, getParam prog (ind + 2) pMode2)
    where
        getParam prog ind pMode =
            let input = prog !! ind
            in if pMode == 1 then input else (prog !! input)

getThrusterSignal :: [Int] -> Int -> [Int] -> Int
getThrusterSignal prog input ps = getThrusterSignalR prog ps input

getThrusterSignalR :: [Int] -> [Int] -> Int -> Int
getThrusterSignalR prog [] input = input
getThrusterSignalR prog (p:ps) input =
    getThrusterSignalR prog ps $ head $ process prog (p:[input])
