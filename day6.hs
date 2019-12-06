import Data.List.Split

data Tree = Node String [Tree]

main = do
    contents <- getContents
    let objects = map (splitOn ")") . lines $ contents
    print . treeChecksum 0 $ makeTree objects "COM"

treeChecksum :: Int -> Tree -> Int
treeChecksum depth (Node _ []) = depth
treeChecksum depth (Node _ children) =
    (+) depth $ sum . map (treeChecksum (depth + 1)) $ children

makeTree :: [[String]] -> String -> Tree 
makeTree list root = Node root (map (makeTree list) $ getChildren list root) 

getChildren :: [[String]] -> String -> [String]
getChildren list parent =
    let append (p:c:[]) r = if (parent == p) then (c:r) else r
    in foldr append [] list
