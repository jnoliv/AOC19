import Intcode

data Direction = Up | Right | Down | Left deriving (Enum)
data Color = Black | White deriving (Enum)
data Panel = Panel (Int, Int) Color

instance Show Direction where
    show Main.Up = "Up"
    show Main.Right = "Right"
    show Main.Down = "Down"
    show Main.Left = "Left"

instance Show Color where
    show Black = "Black"
    show White = "White"

instance Show Panel where
    show (Panel (x,y) c) =
        "(" ++ show x ++ "," ++ show y ++ ":" ++ show c ++ ")"

instance Eq Panel where
    (==) (Panel (x1,y1) _) (Panel (x2,y2) _) = (x1 == x2) && (y1 == y2)

insertPanel :: Panel -> [Panel] -> [Panel]
insertPanel new [] = [new]
insertPanel new (p:ps) =
    if new == p
        then new : ps
        else p : insertPanel new ps

getPanel :: (Int,Int) -> [Panel] -> Panel
getPanel (x,y) [] = Panel (x,y) Black
getPanel (x,y) ((Panel (x2,y2) c):ps) =
    if x == x2 && y == y2
        then Panel (x,y) c
        else getPanel (x,y) ps

panelsToColors :: [Panel] -> [Integer]
panelsToColors ps = map (\(Panel _ c) -> fromIntegral $ fromEnum c) ps

removeDups :: [Panel] -> [Panel]
removeDups [] = []
removeDups (p:ps) =
    if p `elem` ps
        then removeDups ps
        else p : removeDups ps 

main :: IO ()
main = do
    contents <- getContents
    let program = toProgram contents
    let panels = paintHull program
    print . length . removeDups $ panels

runHullPaintingRobot :: [Integer] -> Panel -> Direction -> [Panel] -> [Panel]
runHullPaintingRobot [] _ _ prevPs = []
runHullPaintingRobot (newC:d:is) (Panel (x,y) c) curD prevPs =
    let dirDelta = fromIntegral $ if d == 0 then (-1) else d
        nextD = toEnum $ ((fromEnum curD) + dirDelta) `mod` 4
        nextCoords = case nextD of
            Main.Up    -> (x, y - 1)
            Main.Right -> (x + 1, y)
            Main.Down  -> (x, y + 1)
            Main.Left  -> (x - 1, y)
        nextP = getPanel nextCoords prevPs
        changedPanel = Panel (x,y) (toEnum . fromIntegral $ newC)
        nextPs = insertPanel changedPanel prevPs
    in nextP : runHullPaintingRobot is nextP nextD nextPs

paintHull :: Program -> [Panel]
paintHull program = (Panel (0,0) Black) : init output
    where output = runHullPaintingRobot input (Panel (0,0) Black) Up []
          input = process program $ 0 : panelsToColors output
