{-# LANGUAGE ScopedTypeVariables #-}
module Day7 where

import Data.List
import Data.Foldable
import Data.List.Split

getInput :: IO [Int]
getInput = do
    (map read . splitOn ",") <$> readFile "input/Day7.txt"

-- brute force, but it works
calc :: (Int -> Int) -> [Int] -> Int
calc f is
    | vs <- map (\i -> foldl' (\acc x -> acc + f (abs $ i - x)) 0 is) is
    = head (sort vs)

-- abusing CAF for memoization
sumseq :: [Int]
sumseq = map (sum . flip take [1..]) [0..]

part1 :: [Int] -> Int
part1 = calc id

part2 :: [Int] -> Int
part2 is = calc ((!!) sumseq) is

main :: IO ()
main = do
    i <- getInput
    putStrLn ("Part1: " ++ show (part1 i))
    putStrLn ("Part2: " ++ show (part2 i))
