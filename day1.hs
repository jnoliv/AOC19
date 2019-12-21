calcFuel :: Integral a => a -> a
calcFuel = flip (-) 2 . flip div 3

main :: IO ()
main = do
    contents <- getContents
    let masses = map read . lines $ contents

    let totalFuel = sum . map calcFuel $ masses
    print totalFuel
