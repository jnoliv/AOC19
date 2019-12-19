import Intcode
import Data.List.Split

main = do
    contents <- getContents

    let prog = toProgram contents
    let result = testTractorBeam prog

    putStrLn $ showTractorBeam prog
    print . sum $ result

testTractorBeam :: Program -> [Int]
testTractorBeam prog =
    let input = [[x,y] | y <- [0..49], x <- [0..49]]
        output = map (process prog) input
    in map (fromInteger . head) output

showTractorBeam :: Program -> String
showTractorBeam prog =
    let input = [[x,y] | y <- [0..49], x <- [0..49]]
        output = map (process prog) input
        strOut = map (\l -> if head l == 1 then '#' else '.') output
    in unlines . chunksOf 50 $ strOut
