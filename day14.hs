import Data.List (intersperse)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Tuple (uncurry)
import qualified Data.Map.Strict as Map

data Elem = E Int String
data Reaction = R Elem [Elem]

type MapE = Map.Map String Int
type MapR = Map.Map String Reaction

instance Show Elem where
	show (E n e) = show n ++ " " ++ e

instance Show Reaction where
	show (R output inputs) =
		(concat . intersperse ", " $ map show inputs) ++ " => " ++ show output

getAmount :: Elem -> Int
getAmount (E n _) = n

getResultName :: Reaction -> String
getResultName (R (E _ name) _) = name

getResultAmount :: Reaction -> Int
getResultAmount (R elem _) = getAmount elem

toReaction :: String -> Reaction
toReaction str =
	let [strIns, strOut] = splitOn " => " str
		lIns = splitOn ", " strIns
	in R (toElem strOut) (map toElem lIns)

toElem :: String -> Elem
toElem str =
	let [n, e] = splitOn " " str
	in E (read n) e

calcOre :: MapR -> MapE -> MapE -> Int
calcOre rs curElems leftovers =
	let oreAmount = Map.findWithDefault 0 "ORE" curElems
		oreLeftover = Map.findWithDefault 0 "ORE" leftovers
	in if Map.size curElems == 1 && oreAmount > 0
		then oreAmount + oreLeftover
		else uncurry (calcOre rs) $ react rs curElems leftovers

-- TODO: finish this logic
react :: MapR -> MapE -> MapE -> (MapE, MapE)
react rs curElems leftovers =
	let nextREName  = Map.foldlWithKey (\acc k _ -> if k /= "ORE" then k else acc) "ORE" curElems
		nextR       = fromJust $ Map.lookup nextREName rs
		nextRNeeded = fromJust $ Map.lookup nextREName curElems
		nextRAmount = ceiling $ (fromIntegral nextRNeeded) / (fromIntegral $ getResultAmount nextR)
	in (Map.empty, Map.empty)

main :: IO ()
main = do
	contents <- getContents

	let reactionList = map toReaction $ lines contents
	let reactionMap = Map.fromList $ map (\react -> (getResultName react, react)) reactionList

	let startingElems = Map.singleton "FUEL" 1
	let requiredOre = calcOre reactionMap startingElems Map.empty

	putStrLn $ "Part1: " ++ show requiredOre
	return ()
