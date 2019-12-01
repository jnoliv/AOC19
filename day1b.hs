main = do
    contents <- getContents
    let masses = map (read :: String -> Int) . lines $ contents
    print(sum . (map calcTotalFuel) $ masses)

calcFuel :: Int -> Int
calcFuel mass = (mass `div` 3) - 2

calcTotalFuel :: Int -> Int
calcTotalFuel mass =
    let fuel = calcFuel mass in
        if fuel <= 0
            then 0
            else fuel + (calcTotalFuel fuel)
