import Data.List (isPrefixOf, elemIndex, sortBy)
import Data.Maybe (fromMaybe)
import Data.Function (on)

type Deck = [Int]
type Shuffle = (Deck -> Deck)
data ShuffleInst = NewStack | Cut Int | Increment Int deriving (Show)

toShuffle :: String -> ShuffleInst
toShuffle str
    | "deal into new stack" == str = NewStack
    | "cut " `isPrefixOf` str = Cut . read . drop 4 $ str
    | "deal with increment " `isPrefixOf` str = Increment . read . drop 20 $ str

toFunc :: ShuffleInst -> Shuffle
toFunc NewStack deck = reverse deck
toFunc (Cut n) deck =
    let n' = if n >= 0 then n else length deck + n
    in drop n' deck ++ take n' deck
toFunc (Increment n) deck =
    let len = length deck
        posTo = take len $ iterate (flip mod len . (+ n)) 0
        posFrom = map snd $ sortBy (compare `on` fst) [(posTo !! x, x) | x <- [0..(len - 1)]]
    in map ((!!) deck) posFrom

shuffle :: [Shuffle] -> Deck -> Deck
shuffle ss d = foldl (flip (.)) id ss $ d

main :: IO ()
main = do
    contents <- getContents

    let shuffles = map toFunc . map toShuffle . lines $ contents
    let resDeck = shuffle shuffles [0..10006]

    print . fromMaybe (-1) . elemIndex 2019 $ resDeck
