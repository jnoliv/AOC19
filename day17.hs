import Intcode
import Data.Char
import qualified Data.Map as Map

type Pos = (Int,Int)
data Val = Scaff | Rup | Rright | Rdown | Rleft deriving (Eq, Show)

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
    '#' -> Map.insert (i,j) Scaff pos
    '^' -> Map.insert (i,j) Rup pos
    '>' -> Map.insert (i,j) Rright pos
    'v' -> Map.insert (i,j) Rdown pos
    '<' -> Map.insert (i,j) Rleft pos
    )

main :: IO ()
main = do
    contents <- getContents

    let prog = toProgram contents
    let out = map chr . map fromInteger $ process prog []

    putStrLn out

    let positions = toSet out
    let intersections = findIntersects positions

    print . sum . map (\(x,y) -> x*y) $ intersections

findIntersects :: MapPos -> [Pos]
findIntersects ps = 
    let isMember x y = Map.member (x,y) ps
        isIntersect' x y =
            isMember (x - 1) y &&
            isMember x (y + 1) &&
            isMember (x + 1) y &&
            isMember x (y - 1)
        isIntersect (x,y) = isIntersect' x y
    in filter isIntersect (Map.keys ps)
