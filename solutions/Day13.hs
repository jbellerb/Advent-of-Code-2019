{-# LANGUAGE RecordWildCards #-}

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

data TileType
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  deriving (Show, Eq)

data Screen
  = Screen
      { score :: Integer,
        pixels :: Map (Integer, Integer) TileType
      }
  deriving (Show)

pIntcode :: Parser [Integer]
pIntcode = sepBy (signed space decimal) $ char ','

main :: IO ()
main = do
  contents <- getDayInput 13
  print $ part1 contents
  print $ part2 contents

newScreen :: Screen
newScreen = Screen 0 M.empty

parseTileType :: Integer -> TileType
parseTileType 4 = Ball
parseTileType 3 = Paddle
parseTileType 2 = Block
parseTileType 1 = Wall
parseTileType _ = Empty

processDrawBuffer :: Screen -> [Integer] -> Screen
processDrawBuffer screen [] = screen
processDrawBuffer Screen {..} ((-1) : 0 : newScore : xs) =
  processDrawBuffer (Screen newScore pixels) xs
processDrawBuffer Screen {..} (x : y : tId : xs) =
  processDrawBuffer
    (Screen score updatedScreen)
    xs
  where
    updatedScreen = M.insert (x, y) (parseTileType tId) pixels
processDrawBuffer _ _ = error "Invalid draw command."

testCabinet :: Program -> Screen
testCabinet program = processDrawBuffer newScreen output
  where
    output = runIntcode [] program

part1 :: String -> Int
part1 = M.foldl countBlocks 0 . pixels . testCabinet . parseInput pIntcode
  where
    countBlocks total Block = total + 1
    countBlocks total _ = total

decideMove :: Screen -> Integer
decideMove Screen {..} = signum (ballX - paddleX)
  where
    (ballX, _) = fromJust $ findValue Ball pixels
    (paddleX, _) = fromJust $ findValue Paddle pixels
    findValue value = M.foldlWithKey (isValue value) Nothing
    isValue _ (Just a) _ _ = Just a
    isValue desired Nothing key value
      | value == desired = Just key
      | otherwise = Nothing

playGame :: (Integer, Screen) -> [Integer] -> (Integer, Screen)
playGame (_, screen) stream = (decideMove screen', screen')
  where
    screen' = processDrawBuffer screen stream

part2 :: String -> Integer
part2 input = score $ processDrawBuffer newScreen game
  where
    freePlay = 2 : tail program
    program = parseInput pIntcode input
    game = fst $ last $ interactIntcode playGame (0, newScreen) False freePlay
