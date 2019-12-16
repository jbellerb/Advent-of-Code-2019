import AoC19

import Data.List
import Data.Set (Set)
import qualified Data.Set as S

type Point = (Int, Int)

newtype Ray
  = Ray
      { getRay :: (Int, Int, Int)
      }
  deriving (Show, Eq)

instance Ord Ray where
  (Ray (x1, y1, r1)) `compare` (Ray (x2, y2, r2))
    | x1 == x2 && y1 == y2 = r1 `compare` r2
    | otherwise = t2 `compare` t1
    where
      t1 = atan2 (fromIntegral x1) (fromIntegral (-1 * y1)) :: Double
      t2 = atan2 (fromIntegral x2) (fromIntegral (-1 * y2)) :: Double

main :: IO ()
main = do
  contents <- getDayInput 10
  print $ part1 contents
  print $ part2 contents

findAsteroids :: [String] -> Set Point
findAsteroids = foldl scanRow S.empty . zip [0 ..]
  where
    scanRow set (i, row) = foldl (checkCell i) set $ zip [0 ..] row
    checkCell i set (j, cell) = case cell of
      '#' -> S.insert (j, -1 * i) set
      _ -> set

computeRay :: Point -> Point -> Ray
computeRay (x1, y1) (x2, y2) = Ray (dx `div` r, dy `div` r, r)
  where
    dx = x2 - x1
    dy = y2 - y1
    r = gcd dx dy

findVisible :: Set Point -> Point -> Set (Int, Int)
findVisible set point = foldl addRay S.empty searchSpace
  where
    searchSpace = S.toList $ S.delete point set
    addRay = flip (S.insert . takeTwo . getRay . computeRay point)
    takeTwo (a, b, _) = (a, b)

computePoint :: Point -> Ray -> Point
computePoint (x, y) (Ray (dx, dy, r)) = (x', y')
  where
    x' = x + (dx * r)
    y' = y + (dy * r)

part1 :: String -> Int
part1 asteroidMap =
  maximum $ map (S.size . findVisible points) $
    S.toList
      points
  where
    points = findAsteroids $ lines asteroidMap

sweepOrder :: [Ray] -> [Ray]
sweepOrder [] = []
sweepOrder rays = first ++ sweepOrder next
  where
    dirMatches (Ray (a, b, _)) (Ray (d, e, _)) = a == d && b == e
    moveGroups (a, b) [x] = (a ++ [x], b)
    moveGroups (a, b) (x : xs) = (a ++ [x], b ++ xs)
    moveGroups (a, b) [] = (a, b)
    (first, next) = foldl moveGroups ([], []) $ groupBy dirMatches $ sort rays

part2 :: String -> Int
part2 asteroidMap = 100 * x - 1 * y
  where
    points = findAsteroids $ lines asteroidMap
    station =
      fst $ maximumOn snd $ zipF (S.size . findVisible points) $ S.toList points
    rays = map (computeRay station) $ S.toList $ S.delete station points
    (x, y) = computePoint station $ sweepOrder rays !! 199
