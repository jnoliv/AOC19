import Intcode
import Data.Char (chr, ord)
import System.Environment (getArgs)

solution :: String
solution =
    "south\n\
    \west\n\
    \take hologram\n\
    \south\n\
    \west\n\
    \west\n\
    \take hypercube\n\
    \east\n\
    \east\n\
    \north\n\
    \east\n\
    \south\n\
    \take cake\n\
    \west\n\
    \north\n\
    \take coin\n\
    \south\n\
    \east\n\
    \east\n\
    \south\n\
    \east\n\
    \south\n\
    \south\n"

gameLoop :: Program -> String -> String
gameLoop prog input =
    map (chr . fromInteger) . process prog $ map (toInteger . ord) input

main :: IO ()
main = do
    args <- getArgs
    let useSolution = if length args /= 1 then False else head args == "--use-solution"

    contents <- readFile "day25.in"
    let prog = toProgram contents

    if useSolution
        then putStrLn $ gameLoop prog solution
        else interact $ gameLoop prog
