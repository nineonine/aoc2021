module Day14 where

import Data.List.Split
import Data.Function (on)
import Data.Map.Strict hiding (foldl')
import Data.Foldable (foldl')
import qualified Data.List as L
import qualified Data.Map.Strict as Map

type Input = (Map String Integer, Map String Char, Map Char Integer)

getInput :: IO Input
getInput = (go . lines) <$> readFile "input/Day14.txt"
    where
    go (x:"":xs) = (initPairs x, goRest Map.empty xs, initElems x)
        where goRest acc [] = acc
              goRest acc (y:ys) = case splitOn " -> " y of
                  [k, v] -> goRest (Map.insert k (head v) acc) ys

initPairs :: String -> Map String Integer
initPairs s = foldl' (\acc (k,v) -> Map.alter (adj (+) v) k acc) Map.empty ls
    where ls = zipWith (\a b -> ([a,b], 1)) s (L.tail s)

initElems :: String -> Map Char Integer
initElems s = Map.fromList (L.map (\v -> (head v, fromIntegral $ L.length v)) $ L.group (L.sort s))

adj f x (Just old) = Just (old `f` x)
adj f x Nothing    = Just x

step :: Map String Char
     -> (Map String Integer, Map Char Integer)
     -> String -> Integer
     -> (Map String Integer, Map Char Integer)
step rules (ps, es) r@(x:y:[]) count
    | Just c <- Map.lookup r rules
    , es'    <- Map.alter (adj (+) count) c es
    , ps'    <- Map.alter (adj (-) count) r ps
    , ps''   <- Map.alter (adj (+) count) [x,c] ps'
    , ps'''  <- Map.alter (adj (+) count) [c,y] ps''
    = (ps''', es')

run :: Map String Char -> Int -> (Map String Integer, Map Char Integer)
    -> (Map String Integer, Map Char Integer)
run rules n = go 1
    where
    go m (pairs, elems)
        | m > n = (pairs, elems)
        | next <- Map.foldlWithKey (step rules) (pairs, elems) pairs
        = go (succ m) next

exec :: Int -> Input -> Integer
exec n (pairs, rules, elems)
    | result <- L.map snd $
                L.sortBy (compare `on` snd) $
                Map.toList (snd $ run rules n (pairs, elems))
    , low <- head result
    , high <- last result
    = high - low

part1 :: Input -> Integer
part1 = exec 10

part2 :: Input -> Integer
part2 = exec 40

main :: IO ()
main = do
    i <- getInput
    putStrLn ("Part1: " ++ show (part1 i))
    putStrLn ("Part2: " ++ show (part2 i))
