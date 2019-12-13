import AoC19

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = do
  contents <- getDayInput 6
  print $ part1 contents
  print $ part2 contents

pOrbit :: Parser (String, String)
pOrbit = do
  a <- some alphaNumChar
  _ <- char ')'
  b <- some alphaNumChar
  return (a, b)

countDepth :: Map String String -> String -> Int
countDepth graph node = case parent of
  Just a -> 1 + countDepth graph a
  Nothing -> 1
  where
    parent = graph M.!? node

collectParents :: Map String String -> String -> Set String
collectParents graph node = case parent of
  Just a -> S.singleton node `S.union` collectParents graph a
  Nothing -> S.singleton node
  where
    parent = graph M.!? node

part1 :: String -> Int
part1 input = M.foldl (+) 0 $ M.map (countDepth graph) graph
  where
    graph = M.fromList $ map swap $ parseInput (pOrbit `endBy` newline) input

part2 :: String -> Int
part2 input = S.size pathNodes
  where
    graph = M.fromList $ map swap $ parseInput (pOrbit `endBy` newline) input
    parents = M.map (collectParents graph) graph
    you = fromJust $ parents M.!? "YOU"
    santa = fromJust $ parents M.!? "SAN"
    pathNodes = (you S.\\ santa) `S.union` (santa S.\\ you)
