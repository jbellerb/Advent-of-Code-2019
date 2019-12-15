{-# LANGUAGE TemplateHaskell #-}

module AoC19.Intcode
  ( runIntcode,
    Program,
  )
where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

data CPU
  = CPU
      { _pc :: Integer,
        _relBase :: Integer,
        _inputs :: Maybe [Integer],
        _outputs :: [Integer]
      }
  deriving (Show)

data Instruction
  = Halt
  | Add [Parameter]
  | Mul [Parameter]
  | In [Parameter]
  | Out [Parameter]
  | JT [Parameter]
  | JF [Parameter]
  | CLT [Parameter]
  | CEQ [Parameter]
  | REL [Parameter]
  deriving (Show)

data ParameterMode
  = Pos
  | Imm
  | Rel
  deriving (Show)

data Parameter
  = Parameter
      { mode :: ParameterMode,
        value :: Integer
      }
  deriving (Show)

type Tape = Map Integer Integer

type Program = [Integer]

makeLenses ''CPU

advanceQueue :: Maybe [a] -> (a, Maybe [a])
advanceQueue (Just [x]) = (x, Nothing)
advanceQueue (Just (x : xs)) = (x, Just xs)
advanceQueue _ = error "End of input stream."

resolveParameter :: Parameter -> Tape -> CPU -> Integer
resolveParameter (Parameter Pos a) tape _ = fromMaybe 0 $ M.lookup a tape
resolveParameter (Parameter Imm a) _ _ = a
resolveParameter (Parameter Rel a) tape cpu =
  fromMaybe 0 $ M.lookup (a + (cpu ^. relBase)) tape

resolveTarget :: Parameter -> CPU -> Integer
resolveTarget (Parameter Rel a) cpu = a + (cpu ^. relBase)
resolveTarget (Parameter _ a) _ = a

nextInstruction :: Tape -> CPU -> Instruction
nextInstruction tape cpu = case opcode of
  1 -> Add $ getParameters 3
  2 -> Mul $ getParameters 3
  3 -> In $ getParameters 1
  4 -> Out $ getParameters 1
  5 -> JT $ getParameters 2
  6 -> JF $ getParameters 2
  7 -> CLT $ getParameters 3
  8 -> CEQ $ getParameters 3
  9 -> REL $ getParameters 1
  _ -> Halt
  where
    offset = cpu ^. pc
    cell = fromMaybe 99 $ M.lookup offset tape
    opcode = cell `mod` 100
    getParameters n = take n parameters
    parameters = map constructParameters [1 ..]
    constructParameters n =
      Parameter (decodeMode n) $ fromMaybe 0 $ M.lookup (offset + n) tape
    decodeMode n = case cell `div` (10 ^ (n + 1)) `mod` 10 of
      0 -> Pos
      1 -> Imm
      2 -> Rel
      _ -> error "Invalid parameter mode."

doInstruction :: Tape -> Instruction -> CPU -> (Maybe Tape, CPU)
doInstruction _ Halt cpu = (Nothing, cpu)
doInstruction tape instruction cpu = case instruction of
  Add (a : b : [c]) ->
    ( Just $ M.insert (resolveWrite c) (resolve a + resolve b) tape,
      cpu & pc +~ 4
    )
  Mul (a : b : [c]) ->
    ( Just $ M.insert (resolveWrite c) (resolve a * resolve b) tape,
      cpu & pc +~ 4
    )
  In [a] ->
    ( Just $ M.insert (resolveWrite a) input tape,
      cpu & ((pc +~ 2) . (inputs .~ remaining))
    )
    where
      (input, remaining) = advanceQueue $ cpu ^. inputs
  Out [a] -> (Just tape, cpu & ((pc +~ 2) . (outputs %~ (++ [resolve a]))))
  JT (a : [b])
    | resolve a /= 0 -> (Just tape, cpu & pc .~ resolve b)
    | otherwise -> (Just tape, cpu & pc +~ 3)
  JF (a : [b])
    | resolve a == 0 -> (Just tape, cpu & pc .~ resolve b)
    | otherwise -> (Just tape, cpu & pc +~ 3)
  CLT (a : b : [c])
    | resolve a < resolve b ->
      (Just $ M.insert (resolveWrite c) 1 tape, cpu & pc +~ 4)
    | otherwise ->
      (Just $ M.insert (resolveWrite c) 0 tape, cpu & pc +~ 4)
  CEQ (a : b : [c])
    | resolve a == resolve b ->
      (Just $ M.insert (resolveWrite c) 1 tape, cpu & pc +~ 4)
    | otherwise ->
      (Just $ M.insert (resolveWrite c) 0 tape, cpu & pc +~ 4)
  REL [a] -> (Just tape, cpu & ((pc +~ 2) . (relBase +~ resolve a)))
  _ -> error "This should never happen. (Invalid parsing of instruction.)"
  where
    resolve x = resolveParameter x tape cpu
    resolveWrite x = resolveTarget x cpu

processState :: Maybe Tape -> CPU -> [Integer]
processState Nothing cpu = cpu ^. outputs
processState (Just tape) cpu =
  uncurry processState $
    doInstruction tape instruction cpu
  where
    instruction = nextInstruction tape cpu

runIntcode :: [Integer] -> Program -> [Integer]
runIntcode input program = processState (Just tape) cpu
  where
    tape = M.fromList $ zip [0 ..] program
    cpu = CPU 0 0 (Just input) []
