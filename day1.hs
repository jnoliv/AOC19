main = do
    contents <- getContents
    let masses = map (read :: String -> Int) . lines $ contents
    print(sum . (map calcFuel) $ masses)

calcFuel :: Int -> Int
calcFuel mass = (mass `div` 3) - 2
