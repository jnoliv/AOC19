import Data.List.Split

type Pos = (Int, Int, Int)
type Vel = (Int, Int, Int)
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

potE :: Moon -> Int
potE (Moon pos _) = calcE pos

kinE :: Moon -> Int
kinE (Moon _ vel) = calcE vel

calcE :: (Int,Int,Int) -> Int
calcE (x,y,z) = abs x + abs y + abs z

totalE :: Moon -> Int
totalE m = potE m * kinE m

removeChars :: String -> String -> String
removeChars _ [] = []
removeChars chs (c:cs)
    | c `elem` chs = removeChars chs cs
    | otherwise = c : removeChars chs cs

toMoon :: String -> Moon
toMoon s =
    let [x,y,z] = map read $ splitOn "," s
    in Moon (x,y,z) (0,0,0)

main :: IO ()
main = do
    contents <- getContents
    let numList = removeChars "<xyz= >" $ contents
    let initMoons = map toMoon $ lines numList
    
    --putStrLn . unlines . map show $ initMoons

    let allMoons = iterate step initMoons
    let systemE = sum . map totalE $ (allMoons !! 1000)

    --putStrLn . unlines . map show $ take 17 allMoons
    print systemE

    let periodX = findPeriod . map (map getX) $ allMoons
    let periodY = findPeriod . map (map getY) $ allMoons
    let periodZ = findPeriod . map (map getZ) $ allMoons

    putStrLn $ show periodX ++ ", " ++ show periodY ++ ", " ++ show periodZ
    print (lcm periodX $ lcm periodY periodZ)

step :: [Moon] -> [Moon]
step ms = map move $ map (applyGravity ms) ms

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

delta :: Int -> Int -> Int
delta base x
    | base < x = 1
    | base > x = (-1)
    | otherwise = 0

findPeriod :: [[(Int,Int)]] -> Int
findPeriod (x:xs) = findPeriodAux x xs

findPeriodAux :: [(Int,Int)] -> [[(Int,Int)]] -> Int
findPeriodAux _ [] = (-1)
findPeriodAux base (x:xs)
    | base == x = 1
    | otherwise = 1 + findPeriodAux base xs
