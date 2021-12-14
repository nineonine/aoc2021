module Day13 where

import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.List
import Data.List.Split
import qualified Data.Set as Set
import Data.Matrix

data FoldAction = Vert Int | Hor Int deriving (Show, Eq)
type Paper = Set.Set (Int,Int)
type Dimensions = (Int,Int)

getInput :: IO ([(Int,Int)],[FoldAction])
getInput = (parse [] . lines) <$> readFile "input/Day13.txt" where
    parse acc (x:xs) =
        case x of
            "" -> (acc, parseFoldAction [] xs)
            w -> case splitOn "," w of
                [v1,v2] -> parse (acc++[(read v1, read v2)]) xs
    parseFoldAction acc [] = acc
    parseFoldAction acc (y:ys) =
        case words y of
            ["fold", "along", v] -> case splitOn "=" v of
                [z, n] -> case z of
                    "x" -> parseFoldAction (acc ++ [Vert (read n)]) ys
                    "y" -> parseFoldAction (acc ++ [Hor (read n)]) ys

buildPaper :: [(Int,Int)] -> (Dimensions, Paper)
buildPaper = go ((0,0), Set.empty) where
    go acc [] = acc
    go ((max_x, max_y), paper) ((x,y):rest)
        | paper' <- Set.insert (x,y) paper
        = go ((max max_x x, max max_y y), paper') rest

shift :: Dimensions -> FoldAction -> Maybe Int
shift (w,h) (Hor n)
    | n*2 >= h = Nothing
    | otherwise = Just (abs $ h `div` 2 - n)
shift (w,h) (Vert n)
    | n*2 >= w = Nothing
    | otherwise = Just (abs $ w `div` 2 - n)

shiftCoord :: (Int,Int) -> FoldAction -> (Int,Int)
shiftCoord dims@(w0,h0) a = case a of
    Hor _ -> (w0,h0 + (fromMaybe 0 $ shift dims a))
    Vert _ -> (w0+(fromMaybe 0 $ shift dims a),h0)

alongFoldLine :: Dimensions -> (Int,Int) -> FoldAction -> Bool
alongFoldLine (w,h) (w0,h0) (Hor n) = h0 == n
alongFoldLine (w,h) (w0,h0) (Vert n) = w0 == n

needsTransfer :: (Int,Int) -> FoldAction -> Bool
needsTransfer (w0,h0) (Hor n) = h0 > n
needsTransfer (w0,h0) (Vert n) = w0 > n

newCoords :: Dimensions -> (Int,Int) -> FoldAction -> (Int,Int)
newCoords (w,h) (w0,h0) (Hor n) = (w0, abs $ h - h0)
newCoords (w,h) (w0,h0) (Vert n) =(abs $ w - w0, h0)

newDims :: Dimensions -> FoldAction -> Dimensions
newDims (w,h) (Hor n) = (w, pred h-n)
newDims (w,h) (Vert n) = (pred w-n, h)

foldPaper :: (Dimensions, Paper) -> FoldAction -> (Dimensions, Paper)
foldPaper (dims, p) a = (newDims dims a, Set.foldl' f Set.empty p) where
    f acc k@(w0,h0)
        | alongFoldLine dims k a = acc
        | needsTransfer k a = Set.insert (newCoords dims k a) acc
        | otherwise = Set.insert (shiftCoord k a) acc

part1 :: ([(Int,Int)],[FoldAction]) -> Int
part1 (cs,as)
    | (dims, p) <- buildPaper cs
    = Set.size $ snd $ foldPaper (dims,p) (as !! 0)

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n xs = take n xs : chunks n (drop n xs)

part2 :: ([(Int,Int)],[FoldAction]) -> Matrix Char
part2 (cs,as)
    | (dims, p) <- buildPaper cs
    , r@((w',h'), p') <- foldl' foldPaper (dims,p) as
    , xs <- map (map (\(w0,h0) -> if Set.member (w0,h0) p' then '#' else '.'))
                (chunks (succ w') [(y,x) | x <- [0..h'], y <- [0..w']])
    = fromLists xs

main :: IO ()
main = do
    i <- getInput
    putStrLn ("Part1: " ++ show (part1 i))
    putStrLn ("Part2: " ++ show (part2 i))
