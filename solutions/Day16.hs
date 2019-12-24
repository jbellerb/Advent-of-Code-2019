import AoC19

main :: IO ()
main = do
  contents <- getDayInput 16
  print $ part1 contents
  print $ part2 contents

parseDigits :: String -> [Int]
parseDigits = map (read . (: [])) . init

joinDigits :: [Int] -> Integer
joinDigits = foldl ((+) . (10 *)) 0 . map fromIntegral

getPattern :: Int -> [Int]
getPattern n = tail $ cycle $ concatMap (replicate $ n + 1) [0, 1, 0, -1]

lastDigit :: Int -> Int
lastDigit = (`mod` 10) . abs

phase :: [Int] -> [Int]
phase input = map (lastDigit . calcRow) [0 .. length input - 1]
  where
    calcRow n = sum $ zipWith (*) input $ getPattern n

part1 :: String -> Integer
part1 = joinDigits . take 8 . (!! 100) . iterate phase . parseDigits

repeatList :: Int -> [a] -> [a]
repeatList n list = concatMap (const list) [1 .. n]

cheatingPhase :: Int -> [Int] -> [[Int]]
cheatingPhase n input
  | n > (length input `div` 2) = iterate rippleUp $ drop n input
  | otherwise = error "Offset too low to cheat"
  where
    rippleUp = map (`mod` 10) . scanr1 (+)

part2 :: String -> Integer
part2 input = joinDigits $ take 8 fft
  where
    initial = parseDigits input
    fft = (!! 100) $ cheatingPhase offset $ repeatList 10000 initial
    offset = fromIntegral $ joinDigits $ take 7 initial
