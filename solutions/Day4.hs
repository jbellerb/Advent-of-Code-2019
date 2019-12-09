import AoC19

import qualified Data.Map as M
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

main :: IO ()
main = do
  contents <- getDayInput 4
  print $ part1 contents
  print $ part2 contents

pRange :: Parser [Int]
pRange = do
  start <- decimal
  _ <- char '-'
  end <- decimal
  return [start .. end]

digitsIncrease :: Int -> Bool
digitsIncrease num = checkList individual where individual = digits num

digits :: Int -> [Int]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

checkList :: Ord a => [a] -> Bool
checkList xs =
  fst $ foldl (\(b, x1) x2 -> (b && x1 <= x2, x2)) (True, head xs) xs

findRepeats :: Int -> [Int]
findRepeats num = map snd $ filter (uncurry (==)) parts
  where
    parts = window $ digits num

hasRepeat :: Int -> Bool
hasRepeat = not . null . findRepeats

part1 :: String -> Int
part1 input = length [x | x <- range, digitsIncrease x, hasRepeat x]
  where
    range = parseInput pRange input

checkDouble :: [Int] -> Int -> Bool
checkDouble list x = M.lookup x freq == Just 2 where freq = occurances list

hasProperRepeat :: Int -> Bool
hasProperRepeat num = not (null repeats) && or checkedRepeats
  where
    repeats = findRepeats num
    checkedRepeats = map (checkDouble (digits num)) repeats

part2 :: String -> Int
part2 input =
  length
    [x | x <- range, digitsIncrease x, hasRepeat x, hasProperRepeat x]
  where
    range = parseInput pRange input
