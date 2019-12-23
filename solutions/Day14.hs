import AoC19

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

main :: IO ()
main = do
  contents <- getDayInput 14
  print $ part1 contents
  print $ part2 contents

type Component = (Integer, String)

type ReactionList = Map String (Integer, [Component])

type Leftovers = Map String Integer

pComponent :: Parser Component
pComponent = do
  amount <- decimal
  _ <- space
  compound <- some alphaNumChar
  return (amount, compound)

pReactions :: Parser ReactionList
pReactions = M.fromList <$> pReaction `endBy` newline
  where
    pReaction = do
      ingredients <- sepBy pComponent $ string ", "
      _ <- string " => "
      (amount, compound) <- pComponent
      return (compound, (amount, ingredients))

countReactions ::
  ReactionList -> Leftovers -> String -> Integer -> (Integer, Leftovers)
countReactions _ extra "ORE" needed = (needed, extra)
countReactions graph extra node needed = foldl react (0, extra') ingredients
  where
    (amount, ingredients) = fromJust $ M.lookup node graph
    reused = min needed $ fromMaybe 0 (M.lookup node extra)
    reactions = ceiling $ (needed - reused) % amount
    extra' = M.insertWith (+) node ((reactions * amount) - needed) extra
    react (a, l) (c, n) = (a + a', l')
      where
        (a', l') = countReactions graph l n (reactions * c)

part1 :: String -> Integer
part1 input = fst $ countReactions graph M.empty "FUEL" 1
  where
    graph = parseInput pReactions input

unboundIndex :: (Ord a) => [a] -> a -> Int
unboundIndex xs x = binarySearch xs x (bound `div` 2) (bound + 1)
  where
    bound = uBound 1
    uBound u
      | xs !! u > x = u
      | otherwise = uBound (u * 2)

part2 :: String -> Int
part2 input = unboundIndex possibleReactions 1000000000000
  where
    graph = parseInput pReactions input
    possibleReactions = map (fst . countReactions graph M.empty "FUEL") [0 ..]
