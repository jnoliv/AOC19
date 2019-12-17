import Intcode
import Data.Char
import qualified Data.Map as Map

type Pos = (Int,Int)
data Dir = Up | Right | Down | Left deriving (Enum, Eq, Show)
data Val = Scaff | R Dir deriving (Eq, Show)

type MapPos = Map.Map Pos Val

toSet :: String -> MapPos
toSet str = toSetLines 0 (lines str) Map.empty

toSetLines :: Int -> [String] -> MapPos -> MapPos
toSetLines _ [] ps = ps
toSetLines i (l:ls) ps =
    toSetLines (i + 1) ls $ toSetChars i 0 l ps

toSetChars :: Int -> Int -> String -> MapPos -> MapPos
toSetChars _ _ [] pos = pos
toSetChars i j (c:cs) pos = toSetChars i (j + 1) cs (case c of
    '.' -> pos
    '#' -> Map.insert (j,i) Scaff pos
    '^' -> Map.insert (j,i) (R Main.Up) pos
    '>' -> Map.insert (j,i) (R Main.Right) pos
    'v' -> Map.insert (j,i) (R Main.Down) pos
    '<' -> Map.insert (j,i) (R Main.Left) pos
    )

getFrontPos :: (Pos,Val) -> (Pos,Val)
getFrontPos ((x,y), R dir) = case dir of
    Main.Up    -> ((x, y - 1), R dir)
    Main.Right -> ((x + 1, y), R dir)
    Main.Down  -> ((x, y + 1), R dir)
    Main.Left  -> ((x - 1, y), R dir)

rotate :: (Pos,Val) -> Dir -> (Pos,Val)
rotate (p,R dir) Main.Left  = (p, R (toEnum $ (fromEnum dir - 1) `mod` 4))
rotate (p,R dir) Main.Right = (p, R (toEnum $ (fromEnum dir + 1) `mod` 4))

getLeftPos :: (Pos,Val) -> (Pos,Val)
getLeftPos posVal = getFrontPos $ rotate posVal Main.Left

getRightPos :: (Pos,Val) -> (Pos,Val)
getRightPos posVal = getFrontPos $ rotate posVal Main.Right

main :: IO ()
main = do
    contents <- getContents

    let prog = toProgram contents
    let out = map chr . map fromInteger $ process prog []

    putStrLn out

    let positions = toSet out
    let initPos = head $ filter (\(_, v) -> v /= Scaff) $ Map.toList positions
    let path = drop 2 $ buildPath positions initPos 0

    putStrLn $ "Path: " ++ path

    -- Finding the solution by hand was faster than coding it, lol
    let solution = "A,B,A,B,C,C,B,C,B,A\nR,12,L,8,R,12\nR,8,R,6,R,6,R,8\nR,8,L,8,R,8,R,4,R,4\nn\n"
    let result = last $ process (setAt 0 2 prog) $ map (toInteger . ord) solution

    putStrLn $ "Collected dust: " ++ show result

buildPath :: MapPos -> (Pos,Val) -> Int -> String
buildPath ps curPos nWalked =
    let frontPos = getFrontPos curPos
        hasFront = Map.member (fst frontPos) ps
        leftPos = getLeftPos curPos
        hasLeft = Map.member (fst leftPos) ps
        rightPos = getRightPos curPos
        hasRight = Map.member (fst rightPos) ps
        (rotStr,rotated) = if hasLeft then (",L,", leftPos) else (",R,", rightPos)
    in if hasFront then buildPath ps frontPos (nWalked + 1)
        else if hasLeft || hasRight then show nWalked ++ rotStr ++ buildPath ps rotated 1
            else show nWalked
