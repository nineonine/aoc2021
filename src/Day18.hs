{-# LANGUAGE DeriveFunctor #-}
module Day18 where

import Data.Char
import Data.Foldable (foldl1)
import Data.List (sort, last)

data Pair = P Pair Pair | I Int deriving (Show, Eq)
-- good old Binary Tree Zipper
data Ctx = Top | L Ctx Pair | R Pair Ctx deriving (Show, Eq)
type PairPtr = (Pair, Ctx)

left, right, up, upmost :: PairPtr -> PairPtr
top :: Pair -> PairPtr
left (P l r, c) = (l, L c r)
right (P l r, c) = (r, R l c)
top t = (t, Top)
up (t, L c r) = (P t r, c)
up (t, R l c) = (P l t, c)
upmost l@(p, Top) = l
upmost l = upmost (up l)

getInput :: IO [Pair]
getInput = do
    (map parse . lines) <$> readFile "input/Day18.txt"

parse :: String -> Pair
parse = fst . parse' where
    parse' ('[':rest)
        | (sf1, rest') <- parse' rest
        , (',':rest'') <- rest'
        , (sf2, rest''') <- parse' rest''
        , (']':rest'''') <- rest'''
        = (P sf1 sf2, rest'''')
    parse' xs
        | (ns,rest) <- span isDigit xs
        = (I (read ns), rest)

ppr :: Pair -> String
ppr (P p1 p2) = "[" ++ ppr p1 ++ "," ++ ppr p2 ++ "]"
ppr (I i) = show i

addL :: Int -> Pair -> Pair
addL n (I i) = I (n + i)
addL n (P l r) = P (addL n l) r

addR :: Int -> Pair -> Pair
addR n (I i) = I (n + i)
addR n (P l r) = P l (addR n r)

crawlCtx_L :: (Pair -> Pair) -> Ctx -> Ctx
crawlCtx_L f Top = Top
crawlCtx_L f (L ctx p) = L (crawlCtx_L f ctx) p
crawlCtx_L f (R p ctx) = R (f p) ctx

crawlCtx_R :: (Pair -> Pair) -> Ctx -> Ctx
crawlCtx_R f Top = Top
crawlCtx_R f( R p ctx) = R p (crawlCtx_R f ctx)
crawlCtx_R f (L ctx p) = L ctx (f p)

addToLeft :: Int -> PairPtr -> PairPtr
addToLeft n p@(t, ctx) = case ctx of
    R (I i) c -> (t, R (I (i + n)) c)
    R (P l r) c -> (t, R (P l (addR n r)) c)
    L ctx r -> (t, L (crawlCtx_L (addR n) ctx) r)
    _ -> p

addToRight :: Int -> PairPtr -> PairPtr
addToRight n p@(t, ctx) = case ctx of
    L c (I i) -> (t, L c (I (i + n)))
    L c (P l r) -> (t, L c (P (addL n l) r))
    R l ctx -> (t, R l (crawlCtx_R (addL n) ctx))
    _ -> p

data Executed a = Yes a | Noop a deriving (Show, Functor)

tryExplode :: PairPtr -> Executed PairPtr
tryExplode p = upmost <$> go 0 p where
    go 4 p@(P (I i1) (I i2), ctx)
        | p' <- addToLeft i1 (I 0, ctx)
        = Yes (addToRight i2 p')
    go n p@(P l r, ctx)
        | result <- go (succ n) (left p)
        = case result of
            Yes (l', ctxl) -> Yes (l', ctxl)
            Noop _ -> go (succ n) (right p)
    go n p@(I i, ctx) = Noop p

trySplit :: PairPtr -> Executed PairPtr
trySplit p = upmost <$> go p where
    split (I i) = P (I $ floor $ i'/ 2) (I $ ceiling $ i' / 2)
        where i' = fromIntegral i
    go p@(P l r, ctx)
        | result <- go (top l)
        = case result of
            Yes (l', _) -> Yes (P l' r, ctx)
            Noop _ -> fmap (\(r', _) -> (P l r', ctx)) (go (top r))
    go p@(I i, ctx)
        | i > 9 = Yes (split (I i), ctx)
        | otherwise = Noop p

reduce :: PairPtr -> PairPtr
reduce p = case tryExplode p of
    Yes p' -> reduce p'
    Noop _ -> case trySplit p of
        Yes p' -> reduce p'
        Noop p' -> p'

magnitude :: Pair -> Int
magnitude (I i) = i
magnitude (P p1 p2) = 3 * (magnitude p1) + 2 * (magnitude p2)

part1 :: [Pair] -> Int
part1 = magnitude . foldl1 (\a b -> fst $ reduce $ top $ P a b)

part2 :: [Pair] -> Int
part2 ps | r <- concat [ [[x,y], [y,x] ] | x <- ps, y <- ps ]
    = last $ sort $ map part1 r

main :: IO ()
main = do
    i <- getInput
    putStrLn ("Part1: " ++ show (part1 i))
    putStrLn ("Part2: " ++ show (part2 i))
