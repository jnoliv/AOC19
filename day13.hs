import Intcode
import Data.List.Split (chunksOf)

data Tile = Empty | Wall | Block | Paddle | Ball | Score deriving (Enum, Eq, Show)
data Pos = P (Int,Int) Tile deriving (Show)

getTile :: Pos -> Tile
getTile (P _ t) = t

getScore :: Pos -> Integer
getScore (P (_,n) Score) = toInteger n

toPos :: [Integer] -> Pos
toPos [-1,0,n] = P (-1, fromInteger n) Score
toPos [x,y,t] = P (fromInteger x, fromInteger y) (toEnum . fromInteger $ t)

countTile :: Tile -> [Pos] -> Int
countTile t = length . filter ((== t) . getTile)

followBall :: (Int, Int) -> [Pos] -> [Integer]
followBall _ ((P (x,y) Paddle) : ps) = followBall (x,y) ps
followBall ball@(paddleX, _) ((P (ballX, _) Ball) : ps)
    | ballX < paddleX = (-1) : followBall ball ps
    | ballX > paddleX = 1 : followBall ball ps
    | otherwise = 0 : followBall ball ps
followBall ballPos (p:ps) = followBall ballPos ps

autoplay :: Program -> (Int, Int) -> Integer
autoplay prog initPos =
    let output = process prog input
        ps = map toPos . chunksOf 3 $ output
        input = followBall initPos ps
        scorePs = filter ((== Score) . getTile) ps 
    in getScore . last $ scorePs

main :: IO ()
main = do
    contents <- getContents

    let prog = toProgram contents
    let tiles = map toPos . chunksOf 3 $ process prog []

    let numBlocks = countTile Block tiles

    putStrLn $ "Part1: " ++ show numBlocks

    let progPlay = setAt 0 2 prog
    let paddleInitPos = (\(P p _) -> p) . head . filter ((== Paddle) . getTile) $ tiles
    let score = autoplay progPlay paddleInitPos

    putStrLn $ "Part2: " ++ show score
