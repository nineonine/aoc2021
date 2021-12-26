{-# LANGUAGE MultiWayIf #-}
module Day17 where

import Data.Char (isDigit)
import Data.List.Split (splitOn)

data TargetArea = TA Int Int Int Int deriving Show
type Position = (Int, Int)
type Velocity = (Int, Int)

getInput :: IO TargetArea
getInput =
    parseTA <$> readFile "input/Day17.txt" where
    parseTA s
        | s' <- drop 13 s
        , [xpart, (' ':ypart)] <- splitOn "," s'
        , [x1, x2] <- splitOn ".." (drop 2 xpart)
        , [y1, y2] <- splitOn ".." (drop 2 ypart)
        = TA (read x1) (read x2) (read y1) (read y2)

step :: Position -> Velocity -> (Position, Velocity)
step (x,y) (xvel,yvel)
    | x' <- x + xvel
    , y' <- y + yvel
    , xvel' <- if | xvel > 0 -> pred xvel
                  | xvel < 0 -> succ xvel
                  | otherwise -> 0
    , yvel' <- pred yvel
    = ((x', y'), (xvel', yvel'))

launchProbe :: Position -> Velocity -> [Position]
launchProbe pos vel
    | (pos', vel') <- step pos vel
    = pos': launchProbe pos' vel'

within :: Position -> TargetArea -> Bool
within (xpos, ypos) (TA x1 x2 y1 y2) =
    xpos >= x1 && xpos <= x2 && ypos <= y2 && ypos >= y1

past :: Position -> TargetArea -> Bool
past (xpos, ypos) (TA x1 x2 y1 y2) = ypos < y1

checkLaunch :: TargetArea -> Velocity -> [Position] -> Maybe Int
checkLaunch ta (xvel,yvel) ps = go 0 ps where
    go ymax (pos@(xpos,ypos):rest)
        | pos `within` ta
        = Just ymax
        | otherwise
        = if pos `past` ta
          then Nothing
          else go (max ymax ypos) rest

-- this is quite slow ... can be optimized - we can find 'stable'
-- x (where x velocity reaches 0) to reduce search space
run :: TargetArea -> (Int, Int)
run targetArea@(TA x1 x2 y1 y2) = go 0 0 velocities
    where
    ylim = x2 + (abs y2)
    velocities = [ (x,y) | x <- [1..x2], y <- [y1..ylim] ]
    go maxy c [] = (maxy, c)
    go maxy c (p:rest)
        | Just v <- checkLaunch targetArea p (launchProbe (0,0) p)
        = go (max v maxy) (succ c) rest
        | otherwise = go maxy c rest

main :: IO ()
main = do
    i <- getInput
    let (r1,r2) = run i
    putStrLn ("Part1: " ++ show r1)
    putStrLn ("Part2: " ++ show r2)
