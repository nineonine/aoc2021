{-# LANGUAGE MultiWayIf #-}
module Day8 where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M

getInput :: IO [([String], [String])]
getInput = do
    let parse_line l
            | [patterns, output] <- splitOn " | " l
            = (words patterns, words output)
    (map parse_line . lines) <$> readFile "input/Day8.txt"

part1 :: [([String], [String])] -> Int
part1 = sum . map (count . snd)
    where count = length . filter unique
          unique v = any (\x -> x == length v) [2,3,4,7]

groupByLength :: [String] -> [[String]]
groupByLength = go M.empty where
    go acc [] = M.elems acc
    go acc (x:xs)
        | len <- length x
        , Just vs <- M.lookup len acc
        = go (M.insert len (x:vs) acc) xs
        | otherwise
        = go (M.insert (length x) [x] acc) xs

decode :: ([String], [String]) -> Int
decode (pats,out)
    | [
        [one], [seven], [four],
        two_three_five,
        zero_six_nine,
        [eight]
      ] <- groupByLength (map sort pats)

    , [three] <- filter (\v -> length (v \\ one) == 3) two_three_five
    , [five]  <- filter (\v -> length (v \\ (four \\ one)) == 3
                    && (not (three == v))) two_three_five
    , [two]   <- filter (\v -> not (v == three)
                    && not (v == five)) two_three_five
    , [zero]  <- filter (\v -> length (v \\ (four \\ one)) == 5) zero_six_nine
    , [nine]  <- filter (\v -> length (v \\ four) == 2) zero_six_nine
    , [six]   <- filter (\v -> not (v == zero)
                    && not (v == nine)) zero_six_nine
    , to_digit <- \s ->
        if | s == zero -> "0"  | s == one -> "1"   | s == two -> "2"
           | s == three -> "3" | s == four -> "4"  | s == five -> "5"
           | s == six -> "6"   | s == seven -> "7" | s == eight -> "8"
           | s == nine -> "9"
    = read (concatMap (to_digit . sort) out)

part2 :: [([String], [String])] -> Int
part2 = sum . map decode

main :: IO ()
main = do
    i <- getInput
    putStrLn ("Part1: " ++ show (part1 i))
    putStrLn ("Part2: " ++ show (part2 i))
