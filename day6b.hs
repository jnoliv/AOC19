import Data.List.Split

data Tree = Node String [Tree]

main = do
    contents <- getContents
    let objects = map (splitOn ")") . lines $ contents
    let tree = makeTree objects "COM"
    let Just pathYOU = buildPathToLeaf "YOU" tree
    let Just pathSAN = buildPathToLeaf "SAN" tree
    print $ calcDist pathYOU pathSAN

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

buildPathToLeaf :: String -> Tree -> Maybe [String]
buildPathToLeaf leafName (Node name []) =
    if name == leafName then Just [name] else Nothing
buildPathToLeaf leafName (Node name children) =
    let keepJust a b = if (a == Nothing) then b else a
        found = foldr keepJust Nothing (map (buildPathToLeaf leafName) children)
    in case found of
        Just list -> Just (name:list)
        Nothing -> Nothing

calcDist :: [String] -> [String] -> Int
calcDist (x:xs) (y:ys) =
    if x == y
        then calcDist xs ys
        else (length xs) + (length ys)
