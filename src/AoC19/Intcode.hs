{-# LANGUAGE RecordWildCards #-}

module AoC19.Intcode
  ( runIntcode,
    runIntcode',
    interactIntcode,
    Program,
    CPU,
  )
where

import Data.Function
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

data CPU
  = CPU
      { pc :: Integer,
        relBase :: Integer,
        tape :: Tape,
        inputs :: [Integer],
        writeBuffer :: [Integer]
      }
  deriving (Show)

data Instruction
  = Halt
  | Add Parameter Parameter Parameter
  | Mul Parameter Parameter Parameter
  | In  Parameter
  | Out Parameter
  | JT  Parameter Parameter
  | JF  Parameter Parameter
  | CLT Parameter Parameter Parameter
  | CEQ Parameter Parameter Parameter
  | REL Parameter
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

type State a = (Integer, a)

resolveRead :: CPU -> Parameter -> Integer
resolveRead CPU {..} (Parameter Pos a) = fromMaybe 0 $ M.lookup a tape
resolveRead _        (Parameter Imm a) = a
resolveRead CPU {..} (Parameter Rel a) =
  fromMaybe 0 $ M.lookup (a + relBase) tape

resolveWrite :: CPU -> Parameter -> Integer
resolveWrite CPU {..} (Parameter Rel a) = a + relBase
resolveWrite _        (Parameter _ a) = a

nextInstruction :: CPU -> Instruction
nextInstruction CPU {..} = case opcode of
  1 -> Add (param 1) (param 2) (param 3)
  2 -> Mul (param 1) (param 2) (param 3)
  3 -> In  (param 1)
  4 -> Out (param 1)
  5 -> JT  (param 1) (param 2)
  6 -> JF  (param 1) (param 2)
  7 -> CLT (param 1) (param 2) (param 3)
  8 -> CEQ (param 1) (param 2) (param 3)
  9 -> REL (param 1)
  _ -> Halt
  where
    cell = fromMaybe 99 $ M.lookup pc tape
    opcode = cell `mod` 100
    param n = Parameter (decodeMode n) $ fromMaybe 0 $ M.lookup (pc + n) tape
    decodeMode n = case cell `div` (10 ^ (n + 1)) `mod` 10 of
      0 -> Pos
      1 -> Imm
      2 -> Rel
      _ -> error "Invalid parameter mode."

stepComputer :: CPU -> Either [Integer] (CPU, Maybe [Integer])
stepComputer cpu@CPU {..} = case nextInstruction cpu of
  Halt -> Left writeBuffer
  Add a b c ->
    Right
      ( CPU
          (pc + 4)
          relBase
          (M.insert (resolveW c) (on (+) resolve a b) tape)
          inputs
          writeBuffer,
        Nothing
      )
  Mul a b c ->
    Right
      ( CPU
          (pc + 4)
          relBase
          (M.insert (resolveW c) (on (*) resolve a b) tape)
          inputs
          writeBuffer,
        Nothing
      )
  In a ->
    Right
      ( CPU
          (pc + 2)
          relBase
          (M.insert (resolveW a) (head inputs) tape)
          (tail inputs)
          [],
        Just writeBuffer
      )
  Out a ->
    Right
      (CPU (pc + 2) relBase tape inputs (writeBuffer ++ [resolve a]), Nothing)
  JT a b ->
    Right
      ( CPU
          (if resolve a /= 0 then resolve b else pc + 3)
          relBase
          tape
          inputs
          writeBuffer,
        Nothing
      )
  JF a b ->
    Right
      ( CPU
          (if resolve a == 0 then resolve b else pc + 3)
          relBase
          tape
          inputs
          writeBuffer,
        Nothing
      )
  CLT a b c ->
    Right
      ( CPU
          (pc + 4)
          relBase
          (M.insert (resolveW c) (if on (<) resolve a b then 1 else 0) tape)
          inputs
          writeBuffer,
        Nothing
      )
  CEQ a b c ->
    Right
      ( CPU
          (pc + 4)
          relBase
          (M.insert (resolveW c) (if on (==) resolve a b then 1 else 0) tape)
          inputs
          writeBuffer,
        Nothing
      )
  REL a ->
    Right
      (CPU (pc + 2) (relBase + resolve a) tape inputs writeBuffer, Nothing)
  where
    resolve = resolveRead cpu
    resolveW = resolveWrite cpu

runIntcode :: [Integer] -> Program -> [Integer]
runIntcode input program = stepUntilHalt $ stepComputer initial
  where
    tape = M.fromList $ zip [0 ..] program
    initial = CPU 0 0 tape input []
    stepUntilHalt (Left buffer) = buffer
    stepUntilHalt (Right (cpu, Nothing)) = stepUntilHalt $ stepComputer cpu
    stepUntilHalt (Right (cpu, Just output)) =
      output ++ stepUntilHalt (stepComputer cpu)

runIntcode' :: [Integer] -> Program -> [([Integer], CPU)]
runIntcode' input program = stepUntilHalt [] $ stepComputer initial
  where
    tape = M.fromList $ zip [0 ..] program
    initial = CPU 0 0 tape input []
    stepUntilHalt _ (Left buffer) = [(buffer, CPU 0 0 M.empty [] [])]
    stepUntilHalt outputs (Right (cpu, Nothing)) =
      (outputs, cpu) : stepUntilHalt outputs (stepComputer cpu)
    stepUntilHalt outputs (Right (cpu, Just output)) =
      (outputs', cpu) : stepUntilHalt outputs' (stepComputer cpu)
      where
        outputs' = outputs ++ output

interactIntcode ::
  (State a -> [Integer] -> State a) -> State a -> Program -> [Integer]
interactIntcode f state program = last output
  where
    tape = M.fromList $ zip [0 ..] program
    initial = CPU 0 0 tape input []
    output = stepUntilHalt $ stepComputer initial
    input = map fst $ tail $ scanl f state output
    stepUntilHalt (Left buffer) = [buffer]
    stepUntilHalt (Right (cpu, Nothing)) = stepUntilHalt $ stepComputer cpu
    stepUntilHalt (Right (cpu, Just buffer)) =
      buffer : stepUntilHalt (stepComputer cpu)
