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
  part1 contents
  part2 contents

pIntcode :: Parser [Int]
pIntcode = sepBy (signed space decimal) $ char ','

part1 :: String -> IO ()
part1 input = runIntcode (Just tape) (CPU 0)
  where
    tape = S.fromList $ parseInput pIntcode input

part2 :: String -> IO ()
part2 = part1
