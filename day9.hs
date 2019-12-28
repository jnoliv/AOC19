import Intcode

main :: IO ()
main = do
    contents <- getContents

    let prog = toProgram $ contents

    let out1 = head $ process prog [1]
    let out2 = head $ process prog [2]

    putStrLn $ "Part1: " ++ show out1
    putStrLn $ "Part2: " ++ show out2
