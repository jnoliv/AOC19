import Data.Char

firstDigit :: Int -> Int
firstDigit n = abs(n) `rem` 10

fft :: [Int] -> [Int]
fft input = reverse . map firstDigit . scanl1 (+) $ reverse input

main = do
    contents <- getContents

    let input = map digitToInt $ init contents
    let offset = read . take 7 $ contents

    let signal = drop offset $ concat . replicate 10000 $ input

    let phases = iterate fft signal
    let digits = take 8 (phases !! 100)
    
    putStrLn . concat . map show $ digits
