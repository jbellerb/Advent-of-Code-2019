import AoC19
import AoC19.Intcode

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
  ( decimal,
    signed,
  )

data Direction = N | E | S | W deriving (Show, Enum)

data Robot
  = Robot
      { getCanvas :: Map (Int, Int) Bool,
        getPosition :: (Int, Int),
        getDirection :: Direction
      }
  deriving (Show)

main :: IO ()
main = do
  contents <- getDayInput 11
  print $ part1 contents
  putStr $ unlines $ part2 contents

paintCell :: Integer -> Maybe [Bool] -> Maybe [Bool]
paintCell color Nothing = Just [color == 1]
paintCell color (Just xs) = Just $ xs ++ [color == 1]

rotate :: Integer -> Direction -> Direction
rotate 0 W = N
rotate 1 N = W
rotate 0 a = succ a
rotate 1 a = pred a
rotate _ _ = error "Invalid rotaton"

getCell :: (Int, Int) -> Map (Int, Int) Bool -> Integer
getCell position canvas
  | isNothing rawPoint = 0
  | isJust rawPoint = if fromJust rawPoint then 1 else 0
  where
    rawPoint = M.lookup position canvas
getCell _ _ = 0

camera :: Robot -> Integer
camera (Robot canvas position _) = getCell position canvas

doAction :: Robot -> [Integer] -> [Robot]
doAction (Robot canvas (x, y) direction) (color : turn : xs) =
  robot' : doAction robot' xs
  where
    robot' = Robot canvas' position' direction'
    canvas' = M.insert (x, y) (color == 1) canvas
    direction' = rotate turn direction
    position' = case direction' of
      N -> (x, y + 1)
      E -> (x + 1, y)
      S -> (x, y - 1)
      W -> (x - 1, y)
doAction _ [] = []
doAction _ _ = error "Odd number of robot instructions"

pIntcode :: Parser [Integer]
pIntcode = sepBy (signed space decimal) $ char ','

runRobot :: Bool -> Program -> Robot
runRobot color program = last robotState
  where
    input = runIntcode (map camera robotState) program
    robotState = initialRobot : doAction initialRobot input
    initialRobot = Robot (M.singleton (0, 0) color) (0, 0) N

part1 :: String -> Int
part1 = M.size . getCanvas . runRobot False . parseInput pIntcode

renderCanvas :: Map (Int, Int) Bool -> [String]
renderCanvas points = map drawLine $ reverse [minY .. maxY]
  where
    knownPoints = M.keys points
    minX = fst $ minimumOn fst knownPoints
    maxX = fst $ maximumOn fst knownPoints
    minY = snd $ minimumOn snd knownPoints
    maxY = snd $ maximumOn snd knownPoints
    drawLine y = map (drawPoint y) $ reverse [minX .. maxX]
    drawPoint y x = case M.lookup (x, y) points of
      Just True -> '#'
      _ -> ' '

part2 :: String -> [String]
part2 = renderCanvas . getCanvas . runRobot True . parseInput pIntcode
