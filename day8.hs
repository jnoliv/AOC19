import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

toLayers :: Int -> Int -> String -> [[Int]]
toLayers nCols nRows =
    chunksOf (nCols * nRows) . map digitToInt . init

checkImage :: [[Int]] -> Int
checkImage layers =
    let count0s = map (countIf (== 0)) layers
        layerMin0s = layers !! (indexOfMinimum count0s)
        count1s = countIf (== 1) layerMin0s
        count2s = countIf (== 2) layerMin0s
    in count1s * count2s
    where
        countIf pred = length . filter pred
        indexOfMinimum l = fromJust $ elemIndex (minimum l) l

toImage :: Int -> [[Int]] -> [[Int]]
toImage nCols = chunksOf nCols . colapseLayers

colapseLayers :: [[Int]] -> [Int]
colapseLayers [] = []
colapseLayers layers =
    let remaining = length . head $ layers
    in if remaining == 0
        then []
        else (colapsePixel $ map head layers) : (colapseLayers $ map tail layers)
    where
        colapsePixel = foldr1 (\f b -> if (f == 2) then b else f)

toString :: [[Int]] -> String
toString = unlines . map (map (\c -> if (c /= 1) then ' ' else '#'))

main :: IO ()
main = do
    args <- getArgs
    let (nCols,nRows) = if length args /= 2
                            then (25,6)
                            else (read $ args !! 0, read $ args !! 1)

    contents <- getContents
    let layers = toLayers nCols nRows contents

    putStrLn $ "Part1: " ++ show (checkImage layers)

    let image = toImage nCols layers
    putStrLn $ "Part2: \n" ++ toString image
