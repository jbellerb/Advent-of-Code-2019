module Main where

import AoC19.Day4

main :: IO ()
main = do
  contents <- readFile "input/day4.in"
  print $ part1 contents
  print $ part2 contents
