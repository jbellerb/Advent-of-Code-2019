{-# LANGUAGE RecordWildCards #-}

import AoC19

import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
  ( decimal,
    signed,
  )

data Planet
  = Planet
      { position :: [Int],
        velocity :: [Int]
      }
  deriving (Show, Eq)

pPlanets :: Parser [Planet]
pPlanets = pPlanet `endBy` newline

pPlanet :: Parser Planet
pPlanet = do
  _ <- string "<x="
  x <- pInt
  _ <- string ", y="
  y <- pInt
  _ <- string ", z="
  z <- pInt
  _ <- char '>'
  return $ Planet [x, y, z] [0, 0, 0]

pInt :: Parser Int
pInt = signed space decimal

main :: IO ()
main = do
  contents <- getDayInput 12
  print $ part1 contents
  print $ part2 contents

simulate :: [Planet] -> [Planet]
simulate planets = map gravityThenVelocity planets
  where
    gravityThenVelocity = velocityStep . gravityStep planets

applyGravity :: Planet -> Planet -> Planet
applyGravity (Planet posA velA) (Planet posB _) =
  Planet posA $
    zipWith (+) velA force
  where
    force = map signum $ zipWith (-) posB posA

gravityStep :: [Planet] -> Planet -> Planet
gravityStep planets planet = foldl applyGravity planet planets

velocityStep :: Planet -> Planet
velocityStep (Planet pos vel) = Planet (zipWith (+) pos vel) vel

energy :: Planet -> Int
energy (Planet pos vel) = potential * kinetic
  where
    potential = sum $ map abs pos
    kinetic = sum $ map abs vel

part1 :: String -> Int
part1 = sum . map energy . (!! 1000) . iterate simulate . parseInput pPlanets

getComponent :: Int -> Planet -> (Int, Int)
getComponent n Planet {..} = (position !! n, velocity !! n)

findAxisCycle :: [Planet] -> Int -> Maybe Int
findAxisCycle planets n = fmap fst $ findCycle $ map getAxis history
  where
    history = iterate simulate planets
    getAxis = map (getComponent n)

part2 :: String -> Int
part2 input = foldl lcm 1 $ map (fromJust . findAxisCycle planets) [0 .. 2]
  where
    planets = parseInput pPlanets input
