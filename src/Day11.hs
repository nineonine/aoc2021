{-# LANGUAGE MultiWayIf #-}
module Day11 where

import Data.Char (digitToInt)
import Data.Foldable (foldl')
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type OctopusMap = Map.Map (Int,Int) Int
type FlashedAlready = Set.Set (Int,Int)

mkOctopusMap :: [[Int]] -> OctopusMap
mkOctopusMap vs = go Map.empty (zip [1..] vs)
    where go acc [] = acc
          go acc ((i,x):xs)
                | acc' <- foldl' (\ac (j,v) -> Map.insert (i,j) v ac) acc (zip [1..] x)
                = go acc' xs

getInput :: IO ((Int, Int), OctopusMap)
getInput = do
    ls <- (map (map digitToInt) . lines) <$> readFile "input/Day11.txt"
    let ds@(h, w) = (length ls, length (head ls))
    return (ds, mkOctopusMap ls)

adjacent :: (Int, Int) -> (Int, Int) -> [(Int,Int)]
adjacent (hmax, wmax) (h,w) =
    filter (\(x,y) -> x /= 0 && y /= 0 && x <= hmax && y <= wmax)
    [ (h+1,w), (h-1,w), (h,w+1), (h,w-1)
    , (h-1,w+1), (h-1,w-1), (h+1,w+1), (h+1,w-1)]

flash :: (Int, Int) -> (OctopusMap, FlashedAlready, Int) -> (Int,Int)
      -> (OctopusMap, FlashedAlready, Int)
flash (h,w) s@(octopusMap, flashedAlready, flashes) xy
    | not (Set.member xy flashedAlready)
    , Map.lookup xy octopusMap > Just 9
    , octopusMap' <- Map.update (const (Just 0)) xy octopusMap
    , flashedAlready' <-  Set.insert xy flashedAlready
    , flashes' <- succ flashes
    , adj_octs <- filter (not . flip Set.member flashedAlready') (adjacent (h,w) xy)
    , octopusMap'' <- foldl' (\acc k -> Map.update (Just . succ) k acc) octopusMap' adj_octs
    , next <- filter (\xy0 -> (Map.lookup xy0 octopusMap'') > Just 9) adj_octs
    = foldl' (\acc x0y0 -> flash (h,w) acc x0y0) (octopusMap'', flashedAlready', flashes') next
    | otherwise = s

step :: [(Int,Int)] -> (Int, Int) -> (OctopusMap,Int, [Int]) -> Int -> (OctopusMap, Int, [Int])
step coords (h,w) (octopusMap, initFlashes, allFlashed) stepNo
    | octopusMap' <- Map.map succ octopusMap
    , (mp,_,v) <- foldl' (\acc k -> flash (h,w) acc k) (octopusMap', Set.empty, initFlashes) coords
    = if v - initFlashes == h*w
        then (mp,v, stepNo:allFlashed)
        else (mp,v, allFlashed)

part1 :: ((Int, Int), OctopusMap) -> Int
part1 ((h,w), octopusMap)
    | cs <- [(x,y) | x <- [1..h], y <- [1..w]]
    , (_, r, _) <- foldl' (\acc stepNo -> step cs (h,w) acc stepNo) (octopusMap, 0, []) [1..100]
    = r

part2 :: ((Int, Int), OctopusMap) -> Int
part2 ((h,w), octopusMap) = go octopusMap [1..]
    where cs = [(x,y) | x <- [1..h], y <- [1..w]]
          go octMap (n:ns)
            | (octMap', r, af) <- step cs (h,w) (octMap,0, []) n
            = if null af then go octMap' ns
                         else head af

main :: IO ()
main = do
    i <- getInput
    putStrLn ("Part1: " ++ show (part1 i))
    putStrLn ("Part2: " ++ show (part2 i))
