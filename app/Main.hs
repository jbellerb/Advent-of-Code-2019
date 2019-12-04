module Main where

import AoC19.Day3

main :: IO ()
main = do
  contents <- readFile "input/day3.in"
  print $ part1 contents
  print $ part2 contents
