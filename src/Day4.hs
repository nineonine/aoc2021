{-# LANGUAGE TupleSections #-}
module Day4 where

import Data.List
import Data.List.Split

newtype Grid = Grid [[(Int,Bool)]] deriving (Show)
type Draws = [Int]

getInput :: IO (Draws, [Grid])
getInput = do
    (drawn:_:rest) <- lines <$> readFile "input/Day4.txt"
    let grids = parseRest Nothing [] rest
    return (map read $ splitOn "," drawn, reverse grids)
    where
        mrw :: String -> [(Int, Bool)]
        mrw = map ((,False) . read) . words
        parseRest :: Maybe Grid -> [Grid] -> [String] -> [Grid]
        parseRest g acc [] = case g of
            Nothing -> acc
            Just gr -> gr:acc
        parseRest g acc ("":next) = case g of
            Nothing -> parseRest Nothing acc next
            Just gr -> parseRest Nothing (gr:acc) next
        parseRest g acc (row:rows) = case g of
            Nothing -> parseRest (Just $ Grid [mrw row]) acc rows
            Just (Grid gr) -> parseRest (Just $ Grid $ gr ++ [mrw row]) acc rows

check :: Grid -> Bool
check (Grid g) = any (all snd) g
              || any (all snd) (transpose g)

update :: Int -> Grid -> Grid
update i (Grid g) = Grid (map go g)
    where go [] = []
          go (v@(x,_):xs) = if x == i then (x,True) : go xs
                                      else v : go xs

unmarked :: Grid -> Int
unmarked (Grid g) = sum (map f g)
    where f = sum . map fst . filter (not . snd)

part1 :: Draws -> [Grid] -> Maybe Int
part1 = go where
    go [] gs = Nothing
    go (d:ds) gs
        | gs' <- map (update d) gs
        , r <- find check gs'
        = case r of
            Just winnerGrid ->
                Just (d * (unmarked winnerGrid))
            Nothing -> go ds gs'

part2 :: Draws -> [Grid] -> Maybe Int
part2 = go [] where
    go acc [] gs = Nothing
    go acc (d:ds) gs
        | gs' <- map (update d) gs
        = case span check gs' of
            ([],lost) -> go acc ds lost
            (won,[]) -> Just (d * (unmarked $ head won))
            (won,lost) -> go (acc ++ won) ds lost

main :: IO ()
main = do
    (d,g) <- getInput
    putStrLn ("Part1: " ++ show (part1 d g))
    putStrLn ("Part2: " ++ show (part2 d g))
