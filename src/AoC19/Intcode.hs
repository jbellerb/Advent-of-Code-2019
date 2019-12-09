module AoC19.Intcode
  ( runIntcode,
    CPU (CPU),
    Tape,
  )
where

import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as S

newtype CPU
  = CPU
      { getPC :: Int
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

resolveParameter :: Tape -> Param -> Int
resolveParameter tape (Param False loc) = fromMaybe 0 $ S.lookup loc tape
resolveParameter _ (Param True val) = val

nextInstruction :: Tape -> CPU -> Instruction
nextInstruction tape cpu = case op of
  1 -> Add $ getParams 3
  2 -> Mul $ getParams 3
  3 -> In  $ getParams 1
  4 -> Out $ getParams 1
  5 -> JT  $ getParams 2
  6 -> JF  $ getParams 2
  7 -> CLT $ getParams 3
  8 -> CEQ $ getParams 3
  _ -> Halt
  where
    offset = getPC cpu
    cell = fromMaybe 99 $ S.lookup offset tape
    op = cell `mod` 100
    getParams n = take n params
    params = map constructParams [1 ..]
    constructParams n =
      Param (decodeMode n == 1) $ fromMaybe 0 $ S.lookup (offset + n) tape
    decodeMode n = cell `div` (10 ^ (n + 1)) `mod` 10

doInstruction :: Tape -> Instruction -> CPU -> IO (Maybe Tape, CPU)
doInstruction _ Halt cpu = return (Nothing, cpu)
doInstruction tape instruction cpu = case instruction of
  Add (a : b : [c]) ->
    return
      (Just $ S.update (getValue c) (resolve a + resolve b) tape, CPU $ pc + 4)
  Mul (a : b : [c]) ->
    return
      (Just $ S.update (getValue c) (resolve a * resolve b) tape, CPU $ pc + 4)
  In [a] -> do
    input <- getLine
    return (Just $ S.update (getValue a) (read input) tape, CPU $ pc + 2)
  Out [a] -> do
    print $ resolve a
    return (Just tape, CPU $ pc + 2)
  JT (a : [b])
    | resolve a /= 0 -> return (Just tape, CPU $ resolve b)
    | otherwise -> return (Just tape, CPU $ pc + 3)
  JF (a : [b])
    | resolve a == 0 -> return (Just tape, CPU $ resolve b)
    | otherwise -> return (Just tape, CPU $ pc + 3)
  CLT (a : b : [c])
    | resolve a < resolve b ->
        return
          (Just $ S.update (getValue c) 1 tape, CPU $ pc + 4)
    | otherwise ->
        return (Just $ S.update (getValue c) 0 tape, CPU $ pc + 4)
  CEQ (a : b : [c])
    | resolve a == resolve b ->
        return
          (Just $ S.update (getValue c) 1 tape, CPU $ pc + 4)
    | otherwise ->
        return (Just $ S.update (getValue c) 0 tape, CPU $ pc + 4)
  _ -> error "This should never happen. (Invalid parsing of instruction.)"
  where
    resolve = resolveParameter tape
    pc = getPC cpu

runIntcode :: Maybe Tape -> CPU -> IO ()
runIntcode Nothing _ = return ()
runIntcode (Just tape) cpu =
  uncurry runIntcode =<< doInstruction tape instruction cpu
  where
    instruction = nextInstruction tape cpu
