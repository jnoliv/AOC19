module Intcode (Program, toProgram, process, setAt) where

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.IntMap as Map

type Program = Map.IntMap Integer

toProgram :: String -> Program
toProgram s = 
    let codes = map read . splitOn "," $ s
        combF (map,n) int = (Map.insert n int map, n + 1) 
    in fst $ foldl combF (Map.empty,0) codes

setAt :: Int -> Integer -> Program -> Program
setAt = Map.insert

getAt :: Int -> Program -> Integer
getAt pos = fromMaybe 0 . Map.lookup pos

process :: Program -> [Integer] -> [Integer]
process l input = process' 0 0 input l

process' :: Int -> Int -> [Integer] -> Program -> [Integer]
process' ind relB inL prog =
    let paramsOpcode = fromInteger $ getAt ind prog
        opcode = paramsOpcode `rem` 100
        pMode1 = (paramsOpcode `div` 100) `rem` 10
        pMode2 = (paramsOpcode `div` 1000) `rem` 10
        pMode3 = (paramsOpcode `div` 10000) `rem` 10
    in case opcode of
        -- add first two params and store in third
        1 -> doAlgOp (+) ind relB inL prog pMode1 pMode2 pMode3
        -- multiply first two params and store in third
        2 -> doAlgOp (*) ind relB inL prog pMode1 pMode2 pMode3
        -- read one int from input and store in first param
        3 -> let param1 = getOutputParam prog (ind + 1) relB pMode1
                 value = head inL
            in process' (ind + 2) relB (tail inL) $ setAt param1 value prog 
        -- read from first param and output it
        4 -> let param1 = getParam prog (ind + 1) relB pMode1
             in param1 : process' (ind + 2) relB inL prog
        -- if first param != 0, set ind to second param
        5 -> doJmpOp ((/=) 0) ind relB inL prog pMode1 pMode2
        -- if first param == 0, set ind to second param
        6 -> doJmpOp ((==) 0) ind relB inL prog pMode1 pMode2
        -- if first param < second param, store 1 in third param, else store 0
        7 -> doCmpOp (<) ind relB inL prog pMode1 pMode2 pMode3
        -- if first param == second param, store 1 in third param, else store 0
        8 -> doCmpOp (==) ind relB inL prog pMode1 pMode2 pMode3
        -- adjust relative base
        9 -> let param1 = fromInteger $ getParam prog (ind + 1) relB pMode1 
             in process' (ind + 2) (relB + param1) inL prog
        -- halt execution
        99 -> []
        -- unrecognized opcode
        n -> error $ "Unrecognized opcode " ++ show n ++ "!"
    where
        doAlgOp algOp ind relB inL prog pMode1 pMode2 pMode3 =
            let (param1, param2) = getParams prog ind relB pMode1 pMode2
                param3 = getOutputParam prog (ind + 3) relB pMode3
            in process' (ind + 4) relB inL $ setAt param3 (algOp param1 param2) prog
        doJmpOp jmpOp ind relB inL prog pMode1 pMode2 =
            let (param1, param2) = getParams prog ind relB pMode1 pMode2
                nextInd = if jmpOp param1 then (fromInteger param2) else (ind + 3)
            in process' nextInd relB inL prog
        doCmpOp cmpOp ind relB inL prog pMode1 pMode2 pMode3 =
            let (param1, param2) = getParams prog ind relB pMode1 pMode2
                param3 = getOutputParam prog (ind + 3) relB pMode3
                value = if (param1 `cmpOp` param2) then 1 else 0
            in process' (ind + 4) relB inL $ setAt param3 value prog

getParams :: Program -> Int -> Int -> Int -> Int -> (Integer, Integer)
getParams prog ind relB pMode1 pMode2 =
    (getParam prog (ind + 1) relB pMode1, getParam prog (ind + 2) relB pMode2)

getParam :: Program -> Int -> Int -> Int -> Integer
getParam prog ind relB pMode =
    let input = getAt ind prog
    in case pMode of
        0 -> getAt (fromInteger input) prog
        1 -> input
        2 -> getAt ((fromInteger input) + relB) prog
        n -> error $ "Unrecognized parameter mode " ++ show n ++ "!"

getOutputParam :: Program -> Int -> Int -> Int -> Int
getOutputParam prog ind relB pMode =
    let input = fromInteger $ getAt ind prog
    in case pMode of
        0 -> input
        2 -> relB + input
        n -> error $ "Unrecognized output parameter mode " ++ show n ++ "!"
