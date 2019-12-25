{-# LANGUAGE RecordWildCards #-}

import AoC19
import AoC19.Intcode

import Data.Char
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
  ( decimal,
    signed,
  )

data Direction = N | E | S | W deriving (Show, Eq, Enum)

data Robot
  = Robot
      { position :: (Int, Int),
        direction :: Direction,
        scaffold :: Set (Int, Int)
      }
  deriving (Show)

pIntcode :: Parser [Integer]
pIntcode = sepBy (signed space decimal) $ char ','

main :: IO ()
main = do
  contents <- getDayInput 17
  print $ part1 contents
  print $ part2 contents

asciiDecode :: [Integer] -> String
asciiDecode = map (chr . fromIntegral)

asciiEncode :: String -> [Integer]
asciiEncode = map (fromIntegral . ord)

window3 :: [a] -> [[a]]
window3 (x : y : z : xs) = [x, y, z] : window3 (y : z : xs)
window3 _ = []

getIntersection :: (Eq a) => a -> [[a]] -> [(Int, Int)]
getIntersection tile grid = map fst $ filter (hasPlus . snd) slides
  where
    slides = concat $ zipWith columns [1 ..] $ window3 grid
    columns i xs = zipWith (index i) [1 ..] $ transpose $ map window3 xs
    index y x xs = ((x, y), xs)
    hasPlus [[_, b, _], [d, e, f], [_, h, _]]
      | all (== tile) [b, d, e, f, h] = True
      | otherwise = False
    hasPlus _ = error "Sliding window failed"

part1 :: String -> Int
part1 input = sum $ map (uncurry (*)) $ getIntersection '#' scaffold
  where
    program = parseInput pIntcode input
    scaffold = lines $ init $ asciiDecode $ runIntcode [] program

findPath :: [String] -> Robot
findPath = foldl row (Robot (0, 0) N S.empty) . zip [0, -1 ..]
  where
    row s (y, r) = foldl (column y) s $ zip [0 ..] r
    column y Robot {..} (x, '#') =
      Robot position direction $ S.insert (x, y) scaffold
    column y Robot {..} (x, '^') = Robot (x, y) N $ S.insert (x, y) scaffold
    column _ r _ = r

turn :: Bool -> Direction -> Direction
turn True W = N
turn False N = W
turn True a = succ a
turn False a = pred a

step :: (Int, Int) -> Direction -> (Int, Int)
step (x, y) N = (x, y + 1)
step (x, y) E = (x + 1, y)
step (x, y) S = (x, y - 1)
step (x, y) W = (x - 1, y)

routePath :: Robot -> [String]
routePath Robot {..}
  | S.member f scaffold = "1" : routePath (Robot f direction scaffold)
  | S.member r scaffold = "R" : "1" : routePath (Robot r dr scaffold)
  | S.member l scaffold = "L" : "1" : routePath (Robot l dl scaffold)
  | otherwise = []
  where
    f = position `step` direction
    dr = turn True direction
    r = position `step` dr
    dl = turn False direction
    l = position `step` dl

replace :: (Eq a) => [a] -> Maybe [a] -> [a] -> [[a]]
replace phrase new list
  | null (last splitted) = init $ fromMaybe splitted $ stripPrefix [[]] splitted
  | otherwise = fromMaybe splitted $ stripPrefix [[]] splitted
  where
    splitted = f phrase list []
    feed r = case new of
      Just a -> r : [a]
      Nothing -> [r]
    f _ [] r = [r]
    f p l@(x : xs) r
      | p `isPrefixOf` l = feed r ++ f p (drop (length p) l) []
      | otherwise = f p xs (r ++ [x])

convertWalks :: [String] -> [String]
convertWalks = map total . group
  where
    total xs
      | length xs == 1 = head xs
      | otherwise = show $ length xs

compressPath :: [String] -> [([String], [String], [String])]
compressPath path = validPair $ concatMap buildTriple as
  where
    as = tail $ inits path
    bs a = tail $ inits $ head (replace a Nothing path)
    getC a b
      | all (== possibleC) remaining = Just possibleC
      | otherwise = Nothing
      where
        remaining = concatMap (replace b Nothing) $ replace a Nothing path
        possibleC = head remaining
    buildTriple a = zip3 (repeat a) (bs a) (map (getC a) (bs a))
    validPair [] = []
    validPair ((a, b, Just c) : xs) = (a, b, c) : validPair xs
    validPair ((_, _, Nothing) : xs) = validPair xs

convert :: [String] -> [Integer]
convert = asciiEncode . intercalate ","

fitsSpace :: ([String], [String], [String]) -> Bool
fitsSpace (a, b, c) = all ((<= 20) . length . convert) [a, b, c]

makeMaster :: [String] -> [String] -> [String] -> [String] -> [String]
makeMaster initial a b c = concat replacedC
  where
    replacedA = replace a (Just ["A"]) initial
    replacedB = filter (/= []) $ concatMap (replace b $ Just ["B"]) replacedA
    replacedC = filter (/= []) $ concatMap (replace c $ Just ["C"]) replacedB

part2 :: String -> Integer
part2 input = last $ runIntcode paths $ 2 : tail program
  where
    program = parseInput pIntcode input
    scaffold = lines $ init $ asciiDecode $ runIntcode [] program
    m = convertWalks $ routePath $ findPath scaffold
    (a, b, c) = fromJust $ find fitsSpace $ compressPath m
    master = makeMaster m a b c
    paths = intercalate [10] (map convert [master, a, b, c]) ++ [10, 110, 10]
