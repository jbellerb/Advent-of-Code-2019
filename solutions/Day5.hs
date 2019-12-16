import AoC19
import AoC19.Intcode

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
  ( decimal,
    signed,
  )

main :: IO ()
main = do
  contents <- getDayInput 5
  print $ last $ part1 contents
  print $ last $ part2 contents

pIntcode :: Parser [Integer]
pIntcode = sepBy (signed space decimal) $ char ','

part1 :: String -> [Integer]
part1 = runIntcode [1] . parseInput pIntcode

part2 :: String -> [Integer]
part2 = runIntcode [5] . parseInput pIntcode
