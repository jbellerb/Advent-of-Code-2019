module AoC19
  ( module AoC19,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String

parseInput :: Parser a -> String -> a
parseInput parser input = case parse parser "" input of
  Left e -> error $ errorBundlePretty e
  Right a -> a

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

occurances :: (Ord a) => [a] -> Map a Int
occurances = foldl incrementSet M.empty
  where
    incrementSet set x = M.insertWith (+) x 1 set

window :: [a] -> [(a, a)]
window (x : y : xs) = (x, y) : window (y : xs)
window _ = []
