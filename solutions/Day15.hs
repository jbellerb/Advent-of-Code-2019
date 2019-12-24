{-# LANGUAGE RecordWildCards #-}

import AoC19
import AoC19.Intcode

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
  ( decimal,
    signed,
  )

data Tile = Wall | Path | Goal deriving (Show, Eq)

type Graph = Map (Int, Int) Tile

type Direction = Integer

data Robot
  = Robot
      { position :: [Direction],
        toVisit :: Seq [Direction],
        maze :: Graph
      }
  deriving (Show)

pIntcode :: Parser [Integer]
pIntcode = sepBy (signed space decimal) $ char ','

main :: IO ()
main = do
  contents <- getDayInput 15
  print $ part1 contents
  print $ part2 contents

back :: Direction -> Direction
back 1 = 2
back 2 = 1
back 3 = 4
back 4 = 3
back _ = error "Invalid direction"

pathTo :: [Direction] -> [Direction] -> Direction
pathTo start end
  | common == start = head $ end \\ common
  | otherwise = back $ last $ start \\ common
  where
    common = map fst $ takeWhile (uncurry (==)) $ zip start end

futureVisits :: [Direction] -> [[Direction]]
futureVisits position = map (position ++) possible
  where
    possible = delete [back $ last position] [[1], [2], [3], [4]]

updatePosition :: [Direction] -> Direction -> [Direction]
updatePosition [] move = [move]
updatePosition position move
  | last position == back move = init position
  | otherwise = position ++ [move]

resolveMove :: (Int, Int) -> [Direction] -> (Int, Int)
resolveMove = foldl step
  where
    step (x, y) 1 = (x, y + 1)
    step (x, y) 2 = (x, y - 1)
    step (x, y) 3 = (x + 1, y)
    step (x, y) 4 = (x - 1, y)
    step _ _ = error "Invalid direction"

mapMaze :: (Integer, Robot) -> [Integer] -> (Integer, Robot)
mapMaze (_, Robot {..}) [0] = (direction', Robot position toVisit' maze')
  where
    toVisit' = S.drop 1 toVisit
    direction' = pathTo position $ toVisit' `S.index` 0
    maze' = M.insert (resolveMove (0, 0) $ toVisit `S.index` 0) Wall maze
mapMaze (direction, Robot {..}) [a]
  | position' == goal = (direction', Robot position' toVisit' maze')
  | otherwise = (pathTo position' goal, Robot position' toVisit maze')
  where
    goal = toVisit `S.index` 0
    position' = updatePosition position direction
    direction' = pathTo position' $ toVisit' `S.index` 0
    toVisit' = S.fromList (futureVisits position') S.>< S.drop 1 toVisit
    maze' =
      M.insert (resolveMove (0, 0) goal) (if a == 2 then Goal else Path) maze
mapMaze _ _ = error "Invalid output"

completed :: ([Integer], (Integer, Robot)) -> Bool
completed (_, (_, Robot {..})) = S.length toVisit == 0

getMaze :: [Integer] -> Graph
getMaze program = maze $ snd $ snd $ fromJust $ find completed route
  where
    initialPaths = S.fromList [[1], [2], [3], [4]]
    initialRobot = Robot [] initialPaths M.empty
    route = interactIntcode mapMaze (1, initialRobot) True program

paths :: (Int, Int) -> Graph -> [Direction] -> [[Direction]]
paths start graph position = filter isValid $ map (position ++) possible
  where
    from = case position of
      [] -> []
      _ -> [back $ last position]
    possible = delete from [[1], [2], [3], [4]]
    isValid loc = M.lookup (resolveMove start loc) graph /= Just Wall

bfs :: (Int, Int) -> Robot -> [[Direction]]
bfs _ (Robot _ S.Empty _) = []
bfs start Robot {..} = position : bfs start (Robot position' toVisit' maze)
  where
    toVisit' = S.drop 1 toVisit S.>< S.fromList (paths start maze position)
    position' = toVisit' `S.index` 0

part1 :: String -> Int
part1 input = length $ fromJust $ find isGoal search
  where
    program = parseInput pIntcode input
    graph = getMaze program
    search = bfs (0, 0) $ Robot [] (S.singleton []) graph
    isGoal pos = M.lookup (resolveMove (0, 0) pos) graph == Just Goal

part2 :: String -> Int
part2 input = length $ last search
  where
    program = parseInput pIntcode input
    graph = getMaze program
    goal = fst $ fromJust $ find ((== Goal) . snd) $ M.toAscList graph
    search = bfs goal $ Robot [] (S.singleton []) graph
