import Intcode
import Data.List.Split

data Tile = Empty | Wall | Block | Paddle | Ball deriving (Enum, Eq, Show)
data Pos = P (Int,Int) Tile

main :: IO ()
main = do
    contents <- getContents
    let instr = chunksOf 3 $ process (toProgram contents) []
    let numBlocks = countIf (\(P _ t) -> t == Block) $ toTile instr
    print numBlocks

toTile :: [[Integer]] -> [Pos]
toTile [] = []
toTile ([x,y,t]:xs) =
    P (fromInteger x, fromInteger y) (toEnum . fromInteger $ t) : toTile xs

countIf :: (a -> Bool) -> [a] -> Int
countIf _ [] = 0
countIf f (x:xs)
    | f x = 1 + countIf f xs
    | otherwise = countIf f xs
