import AoC19
import AoC19.Intcode

import Data.Function
import Data.List
import qualified Data.Sequence as S
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

pIntcode :: Parser [Int]
pIntcode = sepBy (signed space decimal) $ char ','

evaluateAmplifiers :: Tape -> [Int] -> Int
evaluateAmplifiers tape phases =
  head $
    foldl (&) [0] [amplify x | x <- phases]
  where
    amplify phase input = runIntcode (Just tape) (makeCPU (phase : input))

loopedAmplifiers :: Tape -> [Int] -> Int
loopedAmplifiers tape phases = last loop
  where
    loop = foldl (&) (0 : loop) [amplify x | x <- phases]
    amplify phase input = runIntcode (Just tape) (makeCPU (phase : input))

part1 :: String -> Int
part1 input = maximum $ map (evaluateAmplifiers tape) $ permutations [0 .. 4]
  where
    tape = S.fromList $ parseInput pIntcode input

part2 :: String -> Int
part2 input = maximum $ map (loopedAmplifiers tape) $ permutations [5 .. 9]
  where
    tape = S.fromList $ parseInput pIntcode input
