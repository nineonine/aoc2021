module Day9 where

import Data.List
import Data.Char (digitToInt)
import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map

type CavesMap = Map.Map (Int,Int) Int

mkCavesMap :: [[Int]] -> CavesMap
mkCavesMap vs = go Map.empty (zip [1..] vs)
    where go acc [] = acc
          go acc ((i,x):xs)
                | acc' <- foldl' (\ac (j,v) -> Map.insert (i,j) v ac) acc (zip [1..] x)
                = go acc' xs

getInput :: IO ((Int, Int), CavesMap)
getInput = do
    ls <- (map (map digitToInt) . lines) <$> readFile "input/Day9.txt"
    let ds@(h, w) = (length ls, length (head ls))
    return (ds, mkCavesMap ls)

adjacent_coords :: (Int, Int) -> (Int, Int) -> [(Int,Int)]
adjacent_coords (hmax, wmax) (h,w) =
    filter (\(x,y) -> x /= 0 && y /= 0 && x <= hmax && y <= wmax)
    [(h+1,w), (h-1,w), (h,w+1), (h,w-1)]

upwardAdjFromPt :: (Int, Int) -> Map.Map (Int,Int) Int -> CavesMap -> (Int,Int) -> Int
                -> Map.Map (Int,Int) Int
upwardAdjFromPt (h,w) acc cavesMap (h0,w0) v0
    | acc' <- Map.insert (h0,w0) v0 acc
    , adj <- adjacent_coords (h,w) (h0,w0)
    , Just adj_vs <- traverse (\k' -> Map.lookup k' cavesMap) adj
    , valid_upward_pts <- filter (\(k,v) -> not (Map.member k acc')
        && v < 9 && v >= v0) (zip adj adj_vs)
    = foldl' (\acc1 ((h1,w1), v1) -> upwardAdjFromPt (h,w) acc1 cavesMap (h1,w1) v1)
            acc' valid_upward_pts

run :: ((Int, Int), CavesMap) -> ([((Int,Int), Int)], [Map.Map (Int,Int) Int])
run ((h,w), cavesMap) = Map.foldlWithKey' find_low_pt ([],[]) cavesMap where
    find_low_pt (acc, basinsAcc) k v
        | adj <- adjacent_coords (h,w) k
        , Just adj_vs <- traverse (\k' -> Map.lookup k' cavesMap) adj
        = if all ((<) v) adj_vs
          then ( (k,v):acc
               , upwardAdjFromPt (h,w) Map.empty cavesMap k v : basinsAcc)
          else (acc, basinsAcc)

part1 :: [((Int,Int), Int)] -> Int
part1 = sum . map (succ . snd)

part2 :: [Map.Map (Int,Int) Int] -> Int
part2 basins
    | [v1,v2,v3] <- take 3 (reverse $ sort (map Map.size basins))
    = v1*v2*v3

main :: IO ()
main = do
    i <- getInput
    let (r, b) = run i
    putStrLn ("Part1: " ++ show (part1 r))
    putStrLn ("Part2: " ++ show (part2 b))
