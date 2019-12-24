import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map

data Bug = Dead | Alive deriving (Enum, Eq)

instance Show Bug where
    show Dead  = "."
    show Alive = "#"

type Pos = (Int,Int)
type Area = Map.Map Pos Bug
type BiodivMap = Map.Map Int Bool

nextState :: Area -> Pos -> Bug -> Bug
nextState area (x,y) state =
    let adjacent = [(x, y - 1), (x + 1, y), (x, y + 1), (x -1, y)]
        count = sum $ map (fromEnum . flip (Map.findWithDefault Dead) area) adjacent
    in case state of
        Dead  -> if count == 1 || count == 2 then Alive else Dead
        Alive -> if count == 1 then Alive else Dead

nextArea :: Area -> Area
nextArea area = Map.mapWithKey (nextState area) area

biodiversity :: Area -> Int
biodiversity area =
    let ascList = map (fromEnum . snd) $ Map.toAscList area
    in sum $ zipWith (*) ascList [2^n | n <- [0..(length ascList - 1)]]

toArea :: String -> Area
toArea str =
    let ls = lines str
        getVal c l = if ((ls !! l) !! c) == '.' then Dead else Alive
        keyValMat = [[((l,c), getVal c l) | c <-[0..(flip (-) 1 . length $ ls !! l)] ] | l <-[0..(length ls - 1)]]
    in Map.fromList $ concat keyValMat

toString :: Int -> Area -> String
toString nCols area =
    unlines . map concat . chunksOf nCols . map (show . snd) $ Map.toAscList area

findMatch :: BiodivMap -> [Area] -> Area
findMatch divMap (area:areas) =
    let curBiodiv = biodiversity area
    in if Map.member curBiodiv divMap
        then area
        else findMatch (Map.insert curBiodiv True divMap) areas

main ::IO ()
main = do
    contents <- getContents

    let initArea = toArea contents
    let match = findMatch Map.empty $ iterate nextArea initArea

    putStrLn $ toString 5 match
    putStrLn $ "Biodiversity: " ++ show (biodiversity match)
