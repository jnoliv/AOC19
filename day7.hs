import Intcode
import Data.List (permutations)
import Data.List.Split (splitOn)

getThrusterSignal :: Program -> Integer -> [Integer] -> Integer
getThrusterSignal prog input [] = input
getThrusterSignal prog input (p:ps) =
    flip (getThrusterSignal prog) ps . fromInteger . head $ process prog (p:[input])

getLoopedThrusterSignal :: Program -> Integer -> [Integer] -> Integer
getLoopedThrusterSignal prog input [s1,s2,s3,s4,s5] = last sig5
    where sig1 = process prog (s1:input:sig5)
          sig2 = process prog (s2:sig1)
          sig3 = process prog (s3:sig2)
          sig4 = process prog (s4:sig3)
          sig5 = process prog (s5:sig4)

main :: IO ()
main = do
    contents <- getContents

    let prog = toProgram contents

    let output1 = maximum . map (getThrusterSignal prog 0) $ permutations [0..4]
    let output2 = maximum . map (getLoopedThrusterSignal prog 0) $ permutations [5..9]

    putStrLn $ "Part1: " ++ show output1
    putStrLn $ "Part2: " ++ show output2
