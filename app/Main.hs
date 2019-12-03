module Main where

import AoC19.Day2

main :: IO ()
main = do
  contents <- readFile "input/day2.in"
  print $ part1 contents
  print $ part2 contents
