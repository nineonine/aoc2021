module Day12 where

import Data.Char
import Data.Foldable
import Data.List (intersperse)
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

data Cave = Start | End | Big String | Small String deriving (Eq, Ord, Show)
type Caves = Map.Map Cave (Set.Set Cave)
type VisitedSmallCaves = Map.Map Cave Int
type SmallCaveVisitCriteria = (VisitedSmallCaves, Cave -> VisitedSmallCaves -> Bool)
type CavePath = [Cave]

caveToStr :: Cave -> String
caveToStr Start = "start"
caveToStr End = "end"
caveToStr (Big s) = s
caveToStr (Small s) = s

mkCavePathStr :: CavePath -> String
mkCavePathStr = concat . intersperse "," . map caveToStr

parseCave :: String -> Cave
parseCave s = case s of
    "start" -> Start
    "end" -> End
    s' -> case all isLower s' of
        True -> Small s'
        _    -> Big s'

setupCavePaths :: Cave -> Cave -> Caves -> Caves
setupCavePaths from to caves
    = let updCave c0 c1 cs = Map.alter (\q -> case q of
                Nothing -> Just (Set.singleton c1)
                Just v -> Just (Set.insert c1 v)
                ) c0 cs
          caves' = updCave to from caves
      in updCave from to caves'

getInput :: IO Caves
getInput = do
    ls <- lines <$> readFile "input/Day12.txt"
    return $ foldl' (\acc v ->
        case splitOn "-" v of
        [x,y] -> setupCavePaths (parseCave x) (parseCave y) acc
        ) Map.empty ls

buildPath :: Caves -> SmallCaveVisitCriteria -> CavePath -> Cave -> [CavePath]
buildPath caves (vsc, alreadyVisited) curPath End = [curPath ++ [End]]
buildPath caves (vsc, alreadyVisited) curPath Start = []
buildPath caves (vsc, alreadyVisited) curPath c@(Small s)
    | alreadyVisited c vsc = []
    | newPath <- curPath ++ [c]
    , vsc' <- Map.alter (\h -> case h of
                Just v -> Just (succ v)
                Nothing -> Just 1
                ) c vsc
    = case Map.lookup c caves of
        Nothing  -> []
        Just nextCaves -> concatMap (buildPath caves (vsc', alreadyVisited) newPath) nextCaves
buildPath caves (vsc, alreadyVisited) curPath c@(Big s)
    | newPath <- curPath ++ [c]
    = case Map.lookup c caves of
        Nothing -> []
        Just nextCaves ->
            concatMap (buildPath caves (vsc, alreadyVisited) newPath) (Set.toList $ nextCaves)

part1 :: Caves -> Int
part1 caves
    | Just nextCaves <- Map.lookup Start caves
    , criteria <- Map.member
    = length (concatMap (buildPath caves (Map.empty, criteria) [Start]) nextCaves)

part2 :: Caves -> Int
part2 caves
    | Just nextCaves <- Map.lookup Start caves
    , criteria <- \c visited ->
        case Map.lookup c visited of
            Nothing -> False
            Just 1  -> if all (==1) (Map.elems visited) then False else True
            _ -> True
    = length (concatMap (buildPath caves (Map.empty, criteria) [Start]) nextCaves)

main :: IO ()
main = do
    i <- getInput
    putStrLn ("Part1: " ++ show (part1 i))
    putStrLn ("Part2: " ++ show (part2 i))
