import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map

data Bug = Dead | Alive deriving (Enum, Eq)

instance Show Bug where
    show Dead  = "."
    show Alive = "#"

type Pos = (Int,Int,Int) -- (level, y, x)
type Area = Map.Map Pos Bug
type BiodivMap = Map.Map Int Bool

getAdjacent :: Pos -> [Pos]
getAdjacent pos = concat $ map ($ pos) [up, right, down, left]

up :: Pos -> [Pos]
up (l,y,x)
    | y == 0           = [(l - 1, 1, 2)]
    | y == 3 && x == 2 = [(l + 1, 4, x') | x' <- [0..4]]
    | otherwise        = [(l, y - 1, x)]

right :: Pos -> [Pos]
right (l,y,x)
    | x == 4           = [(l - 1, 2, 3)]
    | y == 2 && x == 1 = [(l + 1, y', 0) | y' <- [0..4]]
    | otherwise        = [(l, y, x + 1)]

down :: Pos -> [Pos]
down (l,y,x)
    | y == 4           = [(l - 1, 3, 2)]
    | y == 1 && x == 2 = [(l + 1, 0, x') | x' <- [0..4]]
    | otherwise        = [(l, y + 1, x)]

left :: Pos -> [Pos]
left (l,y,x)
    | x == 0           = [(l - 1, 2, 1)]
    | y == 2 && x == 3 = [(l + 1, y', 4) | y' <- [0..4]]
    | otherwise        = [(l, y, x - 1)]

nextState :: Area -> Pos -> Bug -> Bug
nextState area pos state =
    let count = sum . map (fromEnum . flip (Map.findWithDefault Dead) area) $ getAdjacent pos
    in case state of
        Dead  -> if count == 1 || count == 2 then Alive else Dead
        Alive -> if count == 1 then Alive else Dead

-- * Filter the Dead bugs out of the result
-- * Apply next state on all elements of the set and their adjacent cells
nextArea :: Area -> Area
nextArea area = 
    let adj = Map.fromList . map (\k -> (k, Dead)) . concat . map (getAdjacent . fst) $ Map.toList area
    in Map.filter (== Alive). Map.mapWithKey (nextState area) $ Map.union area adj

toArea :: String -> Area
toArea str =
    let ls = lines str
        getVal c l = if ((ls !! l) !! c) == '.' then Dead else Alive
        keyValMat = [[((0,l,c), getVal c l) | c <-[0..(flip (-) 1 . length $ ls !! l)] ] | l <-[0..(length ls - 1)]]
    in Map.delete (0,2,2) . Map.fromList $ concat keyValMat

toString :: Int -> Area -> String
toString nCols area =
    unlines . map concat . chunksOf nCols . map (show . snd) $ Map.toAscList area

countAlive :: Area -> Int
countAlive area = Map.size $ Map.filter (== Alive) area

main ::IO ()
main = do
    contents <- getContents

    let initArea = toArea contents
    let count = countAlive $ (iterate nextArea initArea) !! 200

    putStrLn $ "Alive after 200m: " ++ show count
