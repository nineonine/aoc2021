module Day16 where

import Numeric (readHex)
import Data.Char (digitToInt)
import Text.Printf (printf)
import Data.Maybe (mapMaybe)
import Data.Foldable (foldl')

type Version = Int
type TypeID = Int
data PacketOp = Sum | Product | Min | Max | GT_ | LT_ | EQ_ deriving Show
data Packet = Literal Version Int
            | Operator Version PacketOp [Packet]
            deriving Show

toPacketOp :: Int -> PacketOp
toPacketOp 0 = Sum
toPacketOp 1 = Product
toPacketOp 2 = Min
toPacketOp 3 = Max
toPacketOp 5 = GT_
toPacketOp 6 = LT_
toPacketOp 7 = EQ_

getInput :: IO String
getInput = concat . mapMaybe hexToBin <$> readFile "input/Day16.txt"

hexToBin :: Char -> Maybe String
hexToBin c
  = case readHex [c] of
      (x,_):_ -> Just $ printf "%04b" (x::Int)
      _       -> Nothing

binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

first :: (a -> c) -> (a, b) -> (c, b)
first f (a, b) = (f a, b)

parsePacket :: String -> (Packet, String)
parsePacket s
    | (ver, s') <- first binToInt (splitAt 3 s)
    , (ty_id, s'') <- first binToInt (splitAt 3 s')
    = case ty_id of
        4 -> parseLit ver s''
        _ -> parseOp ver (toPacketOp ty_id) s''

parseLit :: Int -> String -> (Packet, String)
parseLit v str = go 6 [] str where
    go c acc s
        | (chunk, s') <- splitAt 5 s
        , acc' <- acc ++ (tail chunk)
        = case head chunk of
            '1' -> go (c+5) acc' s'
            '0' -> (Literal v (binToInt acc'), s')

parseOp :: Int -> PacketOp -> String -> (Packet, String)
parseOp v op ('0':str)
    | (v, str') <- first binToInt (splitAt 15 str)
    , (next, str'') <- splitAt v str'
    = (go [] next, str'')
      where go acc [] = Operator v op acc
            go acc s
                | (p, s') <- parsePacket s
                = go (acc ++ [p]) s'
parseOp v op ('1':str)
    | (v, str') <- first binToInt (splitAt 11 str)
    = go v [] str'
    where go 0 acc s = (Operator v op acc, s)
          go n acc s
            | (p,s') <- parsePacket s
            = go (pred n) (acc ++ [p]) s'

versionSum :: Packet -> Int
versionSum (Literal v _) = v
versionSum (Operator v _ ps) = v + sum (map versionSum ps)

eval :: Packet -> Int
eval (Literal _ n) = n
eval (Operator _ Sum ps) = sum (map eval ps)
eval (Operator _ Product ps) = product (map eval ps)
eval (Operator _ Min ps) = minimum (map eval ps)
eval (Operator _ Max ps) = maximum (map eval ps)
eval (Operator _ GT_ ps) = if (eval (ps !! 0) > eval (ps !! 1)) then 1 else 0
eval (Operator _ LT_ ps) = if (eval (ps !! 0) < eval (ps !! 1)) then 1 else 0
eval (Operator _ EQ_ ps) = if (eval (ps !! 0) == eval (ps !! 1)) then 1 else 0

part1 :: String -> Int
part1 = versionSum . fst . parsePacket

part2 :: String -> Int
part2 = eval . fst . parsePacket

main :: IO ()
main = do
    i <- getInput
    putStrLn ("Part1: " ++ show (part1 i))
    putStrLn ("Part2: " ++ show (part2 i))
