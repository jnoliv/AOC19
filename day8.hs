import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
    contents <- getContents
    let digits = map digitToInt $ take ((length contents) - 1) contents
    let layers = chunksOf (25 * 6) digits

    let count0s = map (countIf (0 ==)) layers
    let layerMin0s = layers !! (indexOfMinimum count0s)

    let count1s = countIf (1==) layerMin0s
    let count2s = countIf (2==) layerMin0s
    print $ count1s * count2s


countIf :: (a -> Bool) -> [a] -> Int
countIf pred l = length . filter pred $ l

indexOfMinimum :: Ord a => [a] -> Int
indexOfMinimum l = fromMaybe (-1) $ elemIndex (minimum l) l
