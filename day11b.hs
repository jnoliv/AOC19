import Data.List
import Data.List.Split
import Data.Ord
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

instance Eq Color where
    (==) Black Black = True
    (==) White White = True
    (==) White Black = False
    (==) Black White = False

instance Show Panel where
    show (Panel (x,y) c) =
        "(" ++ show x ++ "," ++ show y ++ ":" ++ show c ++ ")"

instance Eq Panel where
    (==) (Panel (x1,y1) _) (Panel (x2,y2) _) = (x1 == x2) && (y1 == y2)

instance Ord Panel where
    (<=) (Panel (x1,y1) _) (Panel (x2,y2) _) =
        if x1 < x2 then True else
            if x1 > x2 then False else y1 <= y2

getX :: Panel -> Int
getX (Panel (x, _) _) = x

getY :: Panel -> Int
getY (Panel (_, y) _) = y

getPrintableColor :: Panel -> Char
getPrintableColor (Panel _ Black) = ' '
getPrintableColor (Panel _ White) = '#'

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
    let panels = removeDups $ paintHull program
    --print . sort $ panels
    putStrLn . panelsToPrintable $ panels

runHullPaintingRobot :: [Integer] -> Panel -> Direction -> [Panel] -> [(Panel, Panel)]
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
    in (nextP, changedPanel) : runHullPaintingRobot is nextP nextD nextPs

paintHull :: Program -> [Panel]
paintHull program = map snd output
    where output = runHullPaintingRobot input (Panel (0,0) White) Up []
          input = process program $ 1 : (panelsToColors . map fst $ output)

panelsToPrintable :: [Panel] -> String
panelsToPrintable ps =
    let minX = minimum . map getX $ ps
        maxX = maximum . map getX $ ps
        minY = minimum . map getY $ ps
        maxY = maximum . map getY $ ps
    in unlines . map (foldr (:) "") $ fillMatrix (minX, minY) (maxX, maxY) ps

fillMatrix :: (Int, Int) -> (Int, Int) -> [Panel] -> [String]
fillMatrix (curX, curY) (maxX, maxY) ps =
    if curX > maxX then [] else
        fillLine (curX, curY) (maxX, maxY) ps : fillMatrix (curX + 1, curY) (maxX, maxY) ps

fillLine :: (Int, Int) -> (Int, Int) -> [Panel] -> String
fillLine (curX, curY) (maxX, maxY) ps =
    if curY > maxY then "" else
       (getPrintableColor $ getPanel (curX, curY) ps): fillLine (curX, curY + 1) (maxX, maxY) ps
