import AoC19

main :: IO ()
main = do
  contents <- getDayInput 1
  print $ part1 contents
  print $ part2 contents

parse :: String -> [Int]
parse = map read . lines

part1 :: String -> Int
part1 = sum . map (\x -> (x `div` 3) - 2) . parse

part2 :: String -> Int
part2 = sum . map calcFuel . parse

calcFuel :: Int -> Int
calcFuel mass
  | mass <= 8 = 0
  | otherwise = fuel + calcFuel fuel
  where
    fuel = (mass `div` 3) - 2
