import Intcode
import Data.Char

springscript :: String
springscript =
    "NOT A J\n\
    \NOT B T\n\
    \OR T J\n\
    \NOT C T\n\
    \OR T J\n\
    \AND D J\n\
    \NOT E T\n\
    \NOT T T\n\
    \OR H T\n\
    \AND T J\n\
    \RUN\n"

main = do
    contents <- getContents

    let prog = toProgram contents
    let input = map (toInteger . ord) springscript
    let out = process prog input

    if last out > 255
        then putStrLn $ "Hull damage: " ++ show (last out)
        else putStrLn $ map (chr . fromIntegral) out
