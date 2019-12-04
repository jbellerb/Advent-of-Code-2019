module AoC19

where

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
