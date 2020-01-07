import Data.List.Split (splitOn)

type Pos = (Int, Int, Int) -- (x, y, z)
type Vel = (Int, Int, Int) -- (vx, vy, vz)
data Moon = Moon Pos Vel

instance Show Moon where
    show (Moon (x,y,z) (vx,vy,vz)) = 
        "pos=<x=" ++ show x ++ ",y=" ++ show y ++ ",z=" ++ show z ++ ">, " ++
            "vel<x=" ++ show vx ++ ",y=" ++ show vy ++ ",z=" ++ show vz ++ ">"

getX :: Moon -> (Int,Int)
getX (Moon (x,_,_) (vx,_,_)) = (x,vx)

getY :: Moon -> (Int,Int)
getY (Moon (_,y,_) (_,vy,_)) = (y,vy)

getZ :: Moon -> (Int,Int)
getZ (Moon (_,_,z) (_,_,vz)) = (z,vz)

move :: Moon -> Moon
move (Moon (x,y,z) (vx,vy,vz)) = Moon (x + vx,y + vy,z + vz) (vx,vy,vz)

calcE :: (Int,Int,Int) -> Int
calcE (x,y,z) = abs x + abs y + abs z

totalE :: Moon -> Int
totalE (Moon pos vel) = calcE pos * calcE vel

toMoon :: String -> Moon
toMoon s =
    let [x,y,z] = map read $ splitOn "," s
    in Moon (x,y,z) (0,0,0)

step :: [Moon] -> [Moon]
step ms = map (move . applyGravity ms) ms

applyGravity :: [Moon] -> Moon -> Moon
applyGravity ms (Moon (x,y,z) (vx,vy,vz)) =
    let deltaVx = velDelta x . map (\(Moon (x,_,_) _) -> x) $ ms
        deltaVy = velDelta y . map (\(Moon (_,y,_) _) -> y) $ ms
        deltaVz = velDelta z . map (\(Moon (_,_,z) _) -> z) $ ms
        newVx = vx + deltaVx
        newVy = vy + deltaVy
        newVz = vz + deltaVz
    in Moon (x, y, z) (newVx, newVy, newVz)

velDelta :: Int -> [Int] -> Int
velDelta base xs = sum . map (delta base) $ xs
    where delta base x
            | base < x = 1
            | base > x = (-1)
            | otherwise = 0

findPeriod :: [[Moon]] -> Int
findPeriod iterMoons =
    let periodX = findIndividualPeriod . map (map getX) $ iterMoons
        periodY = findIndividualPeriod . map (map getY) $ iterMoons
        periodZ = findIndividualPeriod . map (map getZ) $ iterMoons
    in lcm periodX $ lcm periodY periodZ

findIndividualPeriod :: [[(Int,Int)]] -> Int
findIndividualPeriod (x:xs) = findIndividualPeriod' x xs

findIndividualPeriod' :: [(Int,Int)] -> [[(Int,Int)]] -> Int
findIndividualPeriod' _ [] = (-1)
findIndividualPeriod' base (x:xs)
    | base == x = 1
    | otherwise = 1 + findIndividualPeriod' base xs

main :: IO ()
main = do
    contents <- getContents

    let numList = filter (flip notElem "<xyz= >")  contents
    let initMoons = map toMoon $ lines numList

    let allMoons = iterate step initMoons
    let finalMoons = allMoons !! 1000
    let systemE = sum . map totalE $ finalMoons

    putStrLn $ "Part1: " ++ show systemE

    let period = findPeriod allMoons
    
    putStrLn $ "Part2: " ++ show period
