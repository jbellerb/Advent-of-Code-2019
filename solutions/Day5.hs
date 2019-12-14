import AoC19
import AoC19.Intcode

import qualified Data.Sequence as S
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
  ( decimal,
    signed,
  )

main :: IO ()
main = do
  contents <- getDayInput 5
  putStr $ unlines $ map show $ part1 contents
  putStr $ unlines $ map show $ part2 contents

pIntcode :: Parser [Int]
pIntcode = sepBy (signed space decimal) $ char ','

part1 :: String -> [Int]
part1 input = runIntcode (Just tape) cpu
  where
    tape = S.fromList $ parseInput pIntcode input
    cpu = makeCPU [1]

part2 :: String -> [Int]
part2 input = runIntcode (Just tape) cpu
  where
    tape = S.fromList $ parseInput pIntcode input
    cpu = makeCPU [5]
