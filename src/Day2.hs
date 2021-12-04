module Day2 where

import Prelude
import Data.Foldable

getInput :: IO [Mov]
getInput = do
    r <- readFile "input/Day2.txt"
    let parseMov = \v -> case words v of
            ["forward", n] -> Forward (read n)
            ["down", n] -> Down (read n)
            ["up", n] -> Up (read n)
    return (map parseMov (lines r))

data Mov = Forward Int | Down Int | Up Int deriving (Show, Eq)

part1 :: [Mov] -> Int
part1 movs = pos*depth
    where
    (pos, depth) = foldl' go (0, 0) movs
    go (p, d) (Forward n) = (p+n, d)
    go (p, d) (Down n) = (p, d+n)
    go (p, d) (Up n) = (p, max 0 (d-n))

part2 :: [Mov] -> Int
part2 movs = pos*depth
    where
    (pos, depth, _) = foldl' go (0, 0, 0) movs
    go (p, d, a) (Down n) = (p, d, a+n)
    go (p, d, a) (Up n) = (p, d, max 0 (a-n))
    go (p, d, a) (Forward n) = (p+n, d+(a*n), a)

main :: IO ()
main = do
    i <- getInput
    putStrLn ("Part1: " ++ show (part1 i))
    putStrLn ("Part2: " ++ show (part2 i))
