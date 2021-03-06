calcFuel :: Integral a => a -> a
calcFuel = flip (-) 2 . flip div 3

calcWithFuel :: Integral a => a -> a
calcWithFuel = sum . takeWhile (> 0) . tail . iterate calcFuel

main :: IO ()
main = do
    contents <- getContents
    let masses = map read . lines $ contents

    let totalFuel = sum . map calcFuel $ masses
    let totalFuelWithFuel = sum . map calcWithFuel $ masses

    putStrLn $ "Part1: " ++ show totalFuel
    putStrLn $ "Part2: " ++ show totalFuelWithFuel
