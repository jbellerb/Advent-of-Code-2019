import AoC19

import Control.Lens
import Data.List
import Data.List.Split
import Data.Maybe

type Program = [Int]

type Instruction = (Maybe Int, Maybe Int, Maybe Int, Maybe Int)

main :: IO ()
main = do
  contents <- getDayInput 2
  print $ part1 contents
  print $ part2 contents

parse :: String -> Program
parse = map read . splitOn ","

part1 :: String -> Int
part1 =
  (^?! element 0)
    . evaluateProgram 0
    . (element 2 .~ 2)
    . (element 1 .~ 12)
    . parse

setCell :: Int -> Maybe Int -> Program -> Maybe Program
setCell i (Just value) = Just . (element i .~ value)
setCell _ _ = const Nothing

doInstruction :: Instruction -> Program -> Maybe Program
doInstruction (opcode, Just a, Just b, Just c) x =
  setCell
    c
    (action first second)
    x
  where
    first = x ^? element a
    second = x ^? element b
    action = case opcode of
      Just 1 -> (<*>) . ((+) <$>)
      Just 2 -> (<*>) . ((*) <$>)
      _ -> const . const Nothing
doInstruction _ _ = Nothing

evaluateProgram :: Int -> Program -> Program
evaluateProgram pc intcode
  | Just newIntcode <- step = evaluateProgram (pc + 4) newIntcode
  | otherwise = intcode
  where
    opcode = intcode ^? element pc
    a = intcode ^? element (pc + 1)
    b = intcode ^? element (pc + 2)
    c = intcode ^? element (pc + 3)
    step = doInstruction (opcode, a, b, c) intcode

part2 :: String -> Int
part2 program = 100 * noun + verb
  where
    (noun, verb) =
      fromJust $
        find
          (checkResultIs 19690720)
          [(a, b) | a <- [0 .. 99], b <- [0 .. 99]]
    checkResultIs num (pNoun, pVerb) =
      maybe False (num ==)
        . (^? element 0)
        . evaluateProgram 0
        . (element 2 .~ pVerb)
        . (element 1 .~ pNoun)
        $ parse program
