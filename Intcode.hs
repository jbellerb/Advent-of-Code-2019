import AoC19
import AoC19.Intcode

import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
  ( decimal,
    signed,
  )

main :: IO ()
main = do
  args <- getArgs
  program <- readFile $ head args
  interact $ runComputer $ parseInput pIntcode program

pIntcode :: Parser [Integer]
pIntcode = sepBy (signed space decimal) $ char ','

runComputer :: [Integer] -> String -> String
runComputer program = output . process . input
  where
    output = unlines . map show
    process = flip runIntcode program
    input = map read . lines
