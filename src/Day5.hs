{-# LANGUAGE BangPatterns #-}
module Day5 where

import Data.List.Split
import Data.Foldable (foldl')
import qualified Data.Map.Strict as M

data Line = Line (Int, Int) (Int, Int) Direction deriving (Eq, Show)
data Direction = V | H | D deriving (Eq, Show)
type Grid = M.Map (Int,Int) Int

direction :: [Int] -> [Int] -> Direction
direction [x1,y1] [x2,y2]
    | x1 == x2 = V
    | y1 == y2 = H
    | otherwise = D

getInput :: IO [Line]
getInput = do
    ls <- lines <$> readFile "input/Day5.txt"
    pure $ map (\l -> case splitOn " -> " l of
            [e1,e2] -> let l1@[x1,y1] = map read (splitOn "," e1)
                           l2@[x2,y2] = map read (splitOn "," e2)
                       in Line (x1, y1) (x2, y2) (direction l1 l2)
                 ) ls

mark :: Bool -> Grid -> Line -> Grid
mark factorinDiagonal grid l@(Line p1@(x1,y1) p2@(x2,y2) d) = case d of
    V -> foldl' (\m e -> M.alter f (x1,e) m)
                  grid [min y1 y2 .. max y1 y2]
    H -> foldl' (\m e -> M.alter f (e,y1) m)
                 grid [min x1 x2 .. max x1 x2]
    D -> if factorinDiagonal
         then foldl' (\m e -> M.alter f e m) grid (diag p1 p2)
         else grid
    where f (Just old) = Just (succ old)
          f Nothing    = Just 1

part1 :: [Line] -> Int
part1 ls
    | grid <- foldl' (mark False) M.empty ls
    = M.foldl' (\acc v -> if v > 1 then succ acc else acc) 0 grid

part2 :: [Line] -> Int
part2 ls
    | grid <- foldl' (mark True) M.empty ls
    = M.foldl' (\acc v -> if v > 1 then succ acc else acc) 0 grid

diag :: (Int,Int) -> (Int, Int) -> [(Int,Int)]
diag (x0,y0) (x1,y1) = zip xs ys
    where xs = if x0 < x1 then [x0..x1] else [x0, pred x0 .. x1]
          ys = if y0 < y1 then [y0..y1] else [y0, pred y0 .. y1]

main :: IO ()
main = do
    ls <- getInput
    putStrLn ("Part1: " ++ show (part1 ls))
    putStrLn ("Part2: " ++ show (part2 ls))
