module Day10 where

import Data.List (sort)
import Data.Foldable

getInput :: IO [String]
getInput = lines <$> readFile "input/Day10.txt"

data LineState = Good | Corrupted Char Char | Incomplete String deriving (Show, Eq)

validOpenClose :: Char -> Char -> Bool
validOpenClose '(' ')' = True
validOpenClose '{' '}' = True
validOpenClose '[' ']' = True
validOpenClose '<' '>' = True
validOpenClose o   c   = False

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

checkLine :: String -> LineState
checkLine = go []
    where go [] [] = Good
          go stack [] = Incomplete stack
          go [] (y:ys) = go [y] ys
          go stack@(x:xs) (y:ys)
            | y `elem` ['(', '{', '[', '<'] -- open?
            = go (y:stack) ys
            | otherwise -- close
            = case validOpenClose x y of
                True -> go xs ys
                False -> Corrupted x y

complete :: String -> String
complete [] = []
complete (c:cs) = case c of
    '(' -> ')' : complete cs
    '[' -> ']' : complete cs
    '{' -> '}' : complete cs
    '<' -> '>' : complete cs

closeCost :: String -> Int
closeCost = go 0
    where go acc [] = acc
          go acc (c:cs) = case c of
            ')' -> go (1 + (acc*5)) cs
            ']' -> go (2 + (acc*5)) cs
            '}' -> go (3 + (acc*5)) cs
            '>' -> go (4 + (acc*5)) cs

part1 :: [String] -> Int
part1 = foldl' f 0 . map checkLine
    where f acc Good = acc
          f acc (Incomplete _) = acc
          f acc (Corrupted _ v) = acc + (score v)

part2 :: [String] -> Int
part2 ss = (sort results) !! (length results `div` 2)
    where results = foldl' f [] (map checkLine ss)
          f acc Good = acc
          f acc (Incomplete stack) = closeCost (complete stack) : acc
          f acc (Corrupted _ v) = acc

main :: IO ()
main = do
    i <- getInput
    putStrLn ("Part1: " ++ show (part1 i))
    putStrLn ("Part2: " ++ show (part2 i))
