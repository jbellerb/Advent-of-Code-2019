import AoC19

import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char

type Layer = [[Int]]

main :: IO ()
main = do
  contents <- getDayInput 8
  print $ part1 contents
  putStrLn $ renderLayer $ part2 contents

pImage :: Int -> Int -> Parser [Layer]
pImage w h = some (pLayer w h)

pLayer :: Int -> Int -> Parser Layer
pLayer w h = count h (count w pDigit)

pDigit :: Parser Int
pDigit = do
  x <- digitChar
  return $ read [x]

renderLayer :: Layer -> String
renderLayer = unlines . map renderLine
  where
    renderLine = map drawChar
    drawChar 1 = '#'
    drawChar _ = ' '

part1 :: String -> Int
part1 = smallest . map (check . occurances . concat) . parseInput (pImage 25 6)
  where
    check frequency = (frequency M.! 0, frequency M.! 1 * frequency M.! 2)
    smallest = snd . minimumBy fst

mergeLayers :: [Layer] -> Layer
mergeLayers = foldl1 merge
  where
    merge = zipWith (zipWith resolve)
    resolve 2 b = b
    resolve a _ = a

part2 :: String -> Layer
part2 = mergeLayers . parseInput (pImage 25 6)
