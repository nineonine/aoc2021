{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Function (on)
import Data.Char (digitToInt)
import Data.List (sortBy)
import Data.Foldable (foldl')
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Debug.Trace

data NodeMeta = N
    { fscore :: !Int
    , gscore :: !Int
    , hscore :: !Int
    } deriving (Show, Eq)

type RiskMap = Map.Map (Int,Int) Int
type Coords = (Int, Int)

getInput :: IO (RiskMap, Coords)
getInput = do
    i <- lines <$> readFile "input/Day15.txt"
    let ls = zip [1..] i
    let coords = (length $ snd (head ls), length ls)
    let r = foldl'
                (\acc (i,l) ->
                    foldl' (\acc' (j,v) ->
                            Map.insert (i,j) (digitToInt v) acc')
                            acc (zip [1..] l)
                ) Map.empty ls
    return (r, coords)

next :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
next (xmax,ymax) (x,y) =
    filter (\(x,y) -> x /= 0 && y /= 0 && x <= xmax && y <= ymax && (x,y) /= (1,1))
    [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

calc_h :: (Int,Int) -> (Int, Int) -> Int
calc_h (x0,y0) (x1,y1) = (abs (x1 - x0) + abs (y1 - y0))

calc_meta :: (Int,Int) -> Int -> RiskMap -> (Int,Int) -> NodeMeta
calc_meta xy prevrisk rmap coords
    | Just r <- Map.lookup coords rmap
    , !h <- calc_h xy coords
    , !g <- r + prevrisk
    , !f <- g + h
    = N f g h

-- A* search
-- this is embarrassingly slow ... ghci couldn't make it.
run :: (RiskMap, Coords) -> Int
run (rmap, c@(x,y))
    | initNode_h <- calc_h c (1,1)
    , initnodemeta <- N initNode_h 0 initNode_h
    = go [((1,1), initnodemeta)] Set.empty
    where
    go ((nextnode, meta):rest) closed
        | nextnode == c
        = gscore meta
        | Set.member nextnode closed
        = go rest closed
        | nextnodes <- (map
            (\v -> (v, calc_meta c (gscore meta) rmap v))
                $ next c nextnode)
        , neighbours <- filter (\(v, neigh_meta) ->
                gscore neigh_meta >= gscore meta
                    || Set.member v closed
            ) nextnodes
        , open <- sortBy (compare `on` (fscore . snd))
                            (neighbours ++ rest)
        , closed' <- Set.insert nextnode closed
        = go open closed'

scale :: Int -> (RiskMap, Coords) -> (RiskMap, Coords)
scale factor (rmap, (xmax,ymax)) = (rmap', coords')
    where
    coords' = (xmax*factor, ymax*factor)
    rmap' = foldl' (\rmap0 (x0,y0) -> -- tile coord
        Map.foldlWithKey (\rmap1 (x1,y1) risk -> -- risk coord
            let xdiff = pred x0
                ydiff = pred y0
                next_key
                    | xdiff == 0 = (x1, y1 + ydiff * ymax)
                    | ydiff == 0 = (x1 + xdiff * xmax, y1)
                    | otherwise = (x1 + xdiff * xmax, y1 + ydiff * ymax)
                new_risk
                    | next_risk <- risk + xdiff + ydiff
                    = if next_risk > 9 then next_risk `mod` 9 else next_risk
            in Map.insert next_key new_risk rmap1
        ) rmap0 rmap
        ) rmap [ (a,b) | a <- [1..factor], b <- [1..factor], (a,b) /= (1,1)]

part1 :: (RiskMap, Coords) -> Int
part1 = run

part2 :: (RiskMap, Coords) -> Int
part2 = run . scale 5

main :: IO ()
main = do
    i <- getInput
    putStrLn ("Part1: " ++ show (part1 i))
    putStrLn ("Part2: " ++ show (part2 i))
