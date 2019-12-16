import Data.Char

pattern :: [Int]
pattern = [0, 1, 0, (-1)]

getPattern :: Int -> [Int]
getPattern n = tail $ foldr (++) [] [replicate n x | x <- cycle pattern]

firstDigit :: Int -> Int
firstDigit n =
    let dig = n `rem` 10
    in if dig > 0 then dig else -dig

fftDigit :: [Int] -> [Int] -> Int
fftDigit input pat = firstDigit . sum . zipWith (*) input $ pat

fft :: [Int] -> [Int]
fft input = take (length input) [fftDigit input $ getPattern n | n <- [1..]]

main = do
    contents <- getContents
    let input = map digitToInt $ init contents
    let phases = iterate fft input
    let digits = take 8 (phases !! 100)

    putStrLn . concat . map show $ digits
    --print $ take 8 (phases !! 0)
    --print $ take 8 (phases !! 1)
    --print $ take 8 (phases !! 2)
    --print $ take 8 (phases !! 3)
    --print $ take 8 (phases !! 4)
