module AoC19.Day3
  ( part1,
    part2,
  )
where

import AoC19

import Data.Map (Map)
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type MapState = (Map (Int, Int) Int, (Int, Int), Int)

data Move = U Int | L Int | D Int | R Int
  deriving (Read, Show)

pMoves :: Parser [Move]
pMoves = sepBy pMove $ char ','
  where
    pMove = do
      d <- pDirection
      d <$> decimal

pDirection :: Parser (Int -> Move)
pDirection = choice
  [ U <$ char 'U'
  , L <$ char 'L'
  , D <$ char 'D'
  , R <$ char 'R' ]

drawLine :: MapState -> Move -> MapState
drawLine (wireMap, loc, time) move =
  ( foldl (\m (x, t) -> M.insertWith const x t m) wireMap steps,
    fst $ last steps,
    time + len
  )
  where
    steps = map dir [1 .. len]
    (dir, len) = case move of
      U a -> (\x -> ((fst loc, snd loc + x), time + x), a)
      L a -> (\x -> ((fst loc + x, snd loc), time + x), a)
      D a -> (\x -> ((fst loc, snd loc + (-1) * x), time + x), a)
      R a -> (\x -> ((fst loc + (-1) * x, snd loc), time + x), a)

buildMap :: [Move] -> Map (Int, Int) Int
buildMap = fst3 . foldl drawLine (M.empty, (0, 0), 0)

part1 :: String -> Int
part1 =
  M.foldlWithKey smallestDist maxBound
    . foldl1 M.intersection
    . map (buildMap . parseInput pMoves)
    . lines
  where
    smallestDist a (x, y) _
      | a < b = a
      | otherwise = b
      where
        b = abs x + abs y

part2 :: String -> Int
part2 =
  M.foldl smallestDist maxBound
    . foldl1 (M.intersectionWith (+))
    . map (buildMap . parseInput pMoves)
    . lines
  where
    smallestDist = min
