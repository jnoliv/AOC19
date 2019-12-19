import Intcode

main = do
    contents <- getContents

    let prog = toProgram contents
    let res = findSquare (inBeam prog) 100
    print $ fst res * 10000 + snd res

inBeam :: Program -> (Int, Int) -> Bool
inBeam prog (x,y) = (\b -> b == 1) . head $ process prog [toInteger x, toInteger y]

getRow :: ((Int, Int) -> Bool) -> Int -> [(Int,Int)]
getRow inBeam row =
    map fst . takeWhile snd . dropWhile (not . snd) $ [((x,row), inBeam (x,row)) | x <- [0..]]

fitsSquare :: ((Int,Int) -> Bool) -> Int -> (Int,Int) -> Bool
fitsSquare inBeam size (x,y) =
    inBeam (x + size - 1, y) && inBeam (x, y + size - 1)

findSquare :: ((Int,Int) -> Bool) -> Int -> (Int,Int)
findSquare inBeam squareSize =
    let pointsInBeam = concat [getRow inBeam y | y <- [6..]]
    in head . dropWhile (not . fitsSquare inBeam squareSize) $ pointsInBeam
