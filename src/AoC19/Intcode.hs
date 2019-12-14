{-# LANGUAGE TemplateHaskell #-}

module AoC19.Intcode
  ( runIntcode,
    makeCPU,
    CPU (CPU),
    Tape,
  )
where

import Control.Lens
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as S

data CPU
  = CPU
      { _pc :: Int,
        _inputs :: Maybe [Int],
        _outputs :: [Int]
      }
  deriving (Show)

data Instruction
  = Halt
  | Add [Param]
  | Mul [Param]
  | In [Param]
  | Out [Param]
  | JT [Param]
  | JF [Param]
  | CLT [Param]
  | CEQ [Param]
  deriving (Show)

data Param
  = Param
      { getImmediate :: Bool,
        getValue :: Int
      }
  deriving (Show)

type Tape = Seq Int

makeLenses ''CPU

makeCPU :: [Int] -> CPU
makeCPU a = CPU 0 (Just a) []

advanceQueue :: Maybe [a] -> (a, Maybe [a])
advanceQueue (Just [x]) = (x, Nothing)
advanceQueue (Just (x : xs)) = (x, Just xs)
advanceQueue _ = error "End of input stream."

resolveParameter :: Tape -> Param -> Int
resolveParameter tape (Param False loc) = fromMaybe 0 $ S.lookup loc tape
resolveParameter _ (Param True val) = val

nextInstruction :: Tape -> CPU -> Instruction
nextInstruction tape cpu = case opcode of
  1 -> Add $ getParams 3
  2 -> Mul $ getParams 3
  3 -> In $ getParams 1
  4 -> Out $ getParams 1
  5 -> JT $ getParams 2
  6 -> JF $ getParams 2
  7 -> CLT $ getParams 3
  8 -> CEQ $ getParams 3
  _ -> Halt
  where
    offset = cpu ^. pc
    cell = fromMaybe 99 $ S.lookup offset tape
    opcode = cell `mod` 100
    getParams n = take n params
    params = map constructParams [1 ..]
    constructParams n =
      Param (decodeMode n == 1) $ fromMaybe 0 $ S.lookup (offset + n) tape
    decodeMode n = cell `div` (10 ^ (n + 1)) `mod` 10

doInstruction :: Tape -> Instruction -> CPU -> (Maybe Tape, CPU)
doInstruction _ Halt cpu = (Nothing, cpu)
doInstruction tape instruction cpu = case instruction of
  Add (a : b : [c]) ->
    (Just $ S.update (getValue c) (resolve a + resolve b) tape, cpu & pc +~ 4)
  Mul (a : b : [c]) ->
    (Just $ S.update (getValue c) (resolve a * resolve b) tape, cpu & pc +~ 4)
  In [a] ->
    ( Just $ S.update (getValue a) input tape,
      cpu & ((pc +~ 2) . (inputs .~ output))
    )
    where
      (input, output) = advanceQueue $ cpu ^. inputs
  Out [a] -> (Just tape, cpu & ((pc +~ 2) . (outputs %~ (++ [resolve a]))))
  JT (a : [b])
    | resolve a /= 0 -> (Just tape, cpu & pc .~ resolve b)
    | otherwise -> (Just tape, cpu & pc +~ 3)
  JF (a : [b])
    | resolve a == 0 -> (Just tape, cpu & pc .~ resolve b)
    | otherwise -> (Just tape, cpu & pc +~ 3)
  CLT (a : b : [c])
    | resolve a < resolve b ->
      (Just $ S.update (getValue c) 1 tape, cpu & pc +~ 4)
    | otherwise ->
      (Just $ S.update (getValue c) 0 tape, cpu & pc +~ 4)
  CEQ (a : b : [c])
    | resolve a == resolve b ->
      (Just $ S.update (getValue c) 1 tape, cpu & pc +~ 4)
    | otherwise ->
      (Just $ S.update (getValue c) 0 tape, cpu & pc +~ 4)
  _ -> error "This should never happen. (Invalid parsing of instruction.)"
  where
    resolve = resolveParameter tape

runIntcode :: Maybe Tape -> CPU -> [Int]
runIntcode Nothing cpu = cpu ^. outputs
runIntcode (Just tape) cpu =
  uncurry runIntcode $
    doInstruction tape instruction cpu
  where
    instruction = nextInstruction tape cpu
