import Intcode
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map

data Direction = Up | Right | Down | Left deriving (Enum, Show)

type Pos = (Int,Int) -- (y,x)
data Color = Black | White deriving (Enum, Show)

type PanelMap = Map.Map Pos Color 

colorToPrintable :: Color -> Char
colorToPrintable Black = ' '
colorToPrintable White = '#'

turnRight :: Direction -> Direction
turnRight Main.Left = Main.Up
turnRight d = succ d

turnLeft :: Direction -> Direction
turnLeft Main.Up = Main.Left
turnLeft d = pred d

colorToInteger :: Color -> Integer
colorToInteger = toInteger . fromEnum

integerToColor :: Integer -> Color
integerToColor = toEnum . fromInteger

runHullPaintingRobot :: [Integer] -> Pos -> Direction -> PanelMap -> [(Color, (Pos,Color))]
runHullPaintingRobot [] _ _ _ = []
runHullPaintingRobot (newC:d:is) curP@(y,x) curD prevPs =
    let nextD = if (d == 0) then turnLeft curD else turnRight curD
        nextCoords = case nextD of
            Main.Up    -> (y - 1, x)
            Main.Right -> (y, x + 1)
            Main.Down  -> (y + 1, x)
            Main.Left  -> (y, x - 1)
        nextColor = Map.findWithDefault Black nextCoords prevPs
        newColor = integerToColor newC
        nextPs = Map.insert curP newColor prevPs
    in (nextColor, (curP, newColor)) : runHullPaintingRobot is nextCoords nextD nextPs

paintHull :: Color -> Program -> PanelMap
paintHull startColor program =
    let output = runHullPaintingRobot input (0,0) Up $ Map.singleton (0,0) startColor
        outputColor = map (colorToInteger . fst) output
        outputPanels = map snd output
        input = process program $ colorToInteger startColor : outputColor
    in Map.fromList $ ((0,0), startColor) : init outputPanels

toPrintable :: PanelMap -> String
toPrintable panels =
    let pList = Map.toList panels
        ys = map (fst . fst) pList
        xs = map (snd . fst) pList
        (minY, maxY) = (minimum ys, maximum ys)
        (minX, maxX) = (minimum xs, maximum xs)
        colors = [Map.findWithDefault Black (y,x) panels | y <- [minY..maxY], x <- [minX..maxX]]
    in unlines . chunksOf (maxX - minX + 1) . map colorToPrintable $ colors

main :: IO ()
main = do
    contents <- getContents

    let prog = toProgram contents
    let numPaintedPanels = Map.size $ paintHull Black prog

    putStrLn $ "Part1: " ++ show numPaintedPanels

    let paintedPanels = paintHull White prog
    let printablePanels = toPrintable paintedPanels

    putStrLn $ "Part2:\n" ++ printablePanels
