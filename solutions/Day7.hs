import AoC19
import AoC19.Intcode

import Data.Function
import Data.List
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
  ( decimal,
    signed,
  )

main :: IO ()
main = do
  contents <- getDayInput 7
  print $ part1 contents
  print $ part2 contents

pIntcode :: Parser [Integer]
pIntcode = sepBy (signed space decimal) $ char ','

evaluateAmplifiers :: Program -> [Integer] -> Integer
evaluateAmplifiers program phases =
  head $
    foldl (&) [0] [amplify x | x <- phases]
  where
    amplify phase input = runIntcode (phase : input) program

loopedAmplifiers :: Program -> [Integer] -> Integer
loopedAmplifiers program phases = last loop
  where
    loop = foldl (&) (0 : loop) [amplify x | x <- phases]
    amplify phase input = runIntcode (phase : input) program

part1 :: String -> Integer
part1 input =
  maximum $ map (evaluateAmplifiers program) $
    permutations
      [0 .. 4]
  where
    program = parseInput pIntcode input

part2 :: String -> Integer
part2 input = maximum $ map (loopedAmplifiers program) $ permutations [5 .. 9]
  where
    program = parseInput pIntcode input
