module AoC19
  ( module AoC19,
  )
where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String

getDayInput :: Int -> IO String
getDayInput a = readFile $ "input/day" ++ show a ++ ".in"

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

zipF :: (a -> b) -> [a] -> [(a, b)]
zipF f a = zip a $ map f a

minimumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
minimumOn f = foldl1 cmp
  where
    cmp x y
      | f x <= f y = x
      | otherwise = y

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn f = foldl1 cmp
  where
    cmp x y
      | f x >= f y = x
      | otherwise = y

findCycle :: (Eq a) => [a] -> Maybe (Int, Int)
findCycle [] = Nothing
findCycle vals@(x0 : xs) = do
  lambda <- repetitionLength x0 xs 1 1
  mu <- findIndex (uncurry (==)) $ zip vals $ drop lambda vals
  return (lambda, mu)
  where
    repetitionLength _ [] _ _ = Nothing
    repetitionLength t (h : hs) p l
      | t == h = Just l
      | p == l = repetitionLength h hs (p * 2) 1
      | otherwise = repetitionLength t hs p (l + 1)
