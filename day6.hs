import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

data Tree = Node String [Tree]

makeTree :: [[String]] -> String -> Tree 
makeTree list root = Node root (map (makeTree list) $ getChildren list root)

getChildren :: [[String]] -> String -> [String]
getChildren list parent =
    let append [p,c] r = if (parent == p) then (c:r) else r
    in foldr append [] list

treeChecksum :: Int -> Tree -> Int
treeChecksum depth (Node _ []) = depth
treeChecksum depth (Node _ children) =
    (+) depth $ sum . map (treeChecksum (depth + 1)) $ children

buildPathToLeaf :: String -> Tree -> Maybe [String]
buildPathToLeaf leafName (Node name []) =
    if name == leafName then Just [name] else Nothing

buildPathToLeaf nodeName (Node name children) =
    case mapMaybe (buildPathToLeaf nodeName) children of
        []   -> Nothing
        [list] -> Just (name:list)

calcDist :: [String] -> [String] -> Int
calcDist (x:xs) (y:ys) =
    if x == y
        then calcDist xs ys
        else (length xs) + (length ys)

main :: IO ()
main = do
    contents <- getContents

    let objects = map (splitOn ")") . lines $ contents
    let tree = makeTree objects "COM"
    let treeCheck = treeChecksum 0 $ makeTree objects "COM"

    let Just pathYOU = buildPathToLeaf "YOU" tree
    let Just pathSAN = buildPathToLeaf "SAN" tree
    let dist = calcDist pathYOU pathSAN

    putStrLn $ "Part1: " ++ show treeCheck
    putStrLn $ "Part2: " ++ show dist 
