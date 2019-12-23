import Intcode

main :: IO ()
main = do
    contents <- getContents

    let prog = toProgram contents
    let output1 = process prog [1]
    let output2 = process prog [5]

    putStrLn $ "Part1: " ++ show (last output1)
    putStrLn $ "Part2: " ++ show (last output2)
