import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
    contents <- getContents
    let digits = map digitToInt $ take ((length contents) - 1) contents
    let layers = chunksOf (25 * 6) digits
    let image = chunksOf 25 $ map intToDigit $ colapseLayers layers

    let printable = map (\c -> if (c == '0') then ' ' else c) $ unlines image
    putStrLn printable

colapseLayers :: [[Int]] -> [Int]
colapseLayers layers =
    let remaining = length . head $ layers
    in if remaining == 0
        then []
        else (colapsePixel $ map head layers) : (colapseLayers $ map tail layers)

colapsePixel :: [Int] -> Int
colapsePixel l = foldr choosePixel 2 l
    where choosePixel f b = if (f == 2) then b else f
