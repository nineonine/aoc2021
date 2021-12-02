module Day1 where

import Prelude

getInput :: IO [Int]
getInput = do
    r <- readFile "input/Day1.txt"
    return (map read (lines r))

part1 :: [Int] -> Int
part1 i = sum $ zipWith (\a b -> if b > a then 1 else 0) i (tail i)

part2 :: [Int] -> Int
part2 i = sum $ zipWith (\a b -> if b > a then 1 else 0) ws (tail ws)
    where ws = zipWith3 (\a b c -> a+b+c) i (tail i) (drop 2 i)

main :: IO ()
main = do
    i <- getInput
    putStrLn ("Part1: " ++ show (part1 i))
    putStrLn ("Part2: " ++ show (part2 i))
