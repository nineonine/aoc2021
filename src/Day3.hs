module Day3 where

import Prelude
import Data.Char
import Data.Foldable

data Rating = Oxygen | CO2

getInput :: IO [[Int]]
getInput = do
    r <- words <$> readFile "input/Day3.txt"
    return (map (map digitToInt) r)

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

gamma :: [[Int]] -> [Int]
gamma = go [] where
    go acc ([]:rest) = acc
    go acc ((x:xs):rest)
        | heads <- x : map head rest
        , (z, o) <- count (0, 0) heads
        , v <- if z > o then 0 else 1
        = go (acc ++ [v]) (xs : map tail rest)

    count acc [] = acc
    count (z,o) (y:ys) =
        if y == 0 then count (succ z, o) ys
                  else count (z, succ o) ys

part1 :: [[Int]] -> Int
part1 i = let gamma_value = gamma i
              epsilon_value = map (\i -> if i == 0 then 1 else 0) gamma_value
          in (toDec $ concatMap show gamma_value) * (toDec $ concatMap show epsilon_value)

spann :: [[Int]] -> ([[Int]], Int, [[Int]], Int)
spann = go ([],0,[],0) where
    go acc [] = acc
    go (zs,zc,os,oc) (x:xs)
        | (y:ys) <- x
        , y == 0    = go (x:zs, succ zc, os, oc) xs
        | otherwise = go (zs, zc, x:os, succ oc) xs

measure :: Rating -> [[Int]] -> [Int]
measure = msr [] where
    msr :: [Int] -> Rating -> [[Int]] -> [Int]
    msr acc _ [] = acc
    msr acc _ [v] = acc ++ v
    msr acc rating vs = msr (acc ++ [n]) rating (map tail nextPass) where
        (zs,zc,os,oc) = spann vs
        (nextPass,n) = case rating of
            Oxygen -> if oc >= zc then (os,1) else (zs,0)
            CO2    -> if zc <= oc then (zs,0) else (os,1)

part2 :: [[Int]] -> Int
part2 i = let oxy = measure Oxygen i
              co2 = measure CO2 i
          in (toDec $ concatMap show oxy) * (toDec $ concatMap show co2)

main :: IO ()
main = do
    i <- getInput
    putStrLn ("Part1: " ++ show (part1 i))
    putStrLn ("Part2: " ++ show (part2 i))
