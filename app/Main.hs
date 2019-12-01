module Main where

import AOC19.Day1 as D1

main :: IO ()
main = do
  contents <- readFile "input/day1.in"
  print $ D1.part1 contents
  print $ D1.part2 contents
