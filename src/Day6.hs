module Day6 where

import Data.List.Split
import Data.Foldable
import qualified Data.Map.Strict as M

type Memo = M.Map (Int,Int) Int

getInput :: IO [Int]
getInput =
    (map read . splitOn ",") <$> readFile "input/Day6.txt"

lIFESPAN :: Int
lIFESPAN = 7

f (memo, s) v | (memo', s') <- calc memo v = (memo', s + s')

-- For roots need to increment initFishtimer (so we do it in caller)
calc :: Memo -> (Int, Int) -> (Memo, Int)
calc memo k@(days, initfishtimer)
    | Just v <- M.lookup k memo
    = (memo, v)
    | days_after_first_span <- days - initfishtimer
    , childSpans <- days_after_first_span `div` lIFESPAN
    , childrenSpanDays <-
            take childSpans
                       [pred days_after_first_span
                       ,pred days_after_first_span-lIFESPAN..]
    , r <- childSpans + 1 -- account for first born fish
    , (memo', chidrenResults)
        <- foldl' f (memo, 0) (zip childrenSpanDays (repeat 8))
    , result <- r + chidrenResults
    = (M.insert k result memo', result)


part1 :: [Int] -> Int
part1 i = snd (foldl' f (M.empty, length i) (zip (repeat 80) $ map succ i))

part2 :: [Int] -> Int
part2 i = snd (foldl' f (M.empty, length i) (zip (repeat 256) $ map succ i))

main :: IO ()
main = do
    i <- getInput
    putStrLn ("Part1: " ++ show (part1 i))
    putStrLn ("Part2: " ++ show (part2 i))
