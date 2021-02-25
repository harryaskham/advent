{-# LANGUAGE TemplateHaskell #-}

module TwentyNineteen.Intcode where

import Control.Lens (makeLenses, (%~), (.~), (^.))
import Data.Function ((&))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)

data Opcode
  = Add
  | Mul
  | Input
  | Output
  | JumpIfTrue
  | JumpIfFalse
  | LessThan
  | Equals
  | AdjustBase
  | Terminate
  deriving (Show, Eq)

data Mode
  = Positional
  | Immediate
  | Relative
  deriving (Show, Eq)

type Param = Integer

type Program = M.Map Integer Integer

type Counter = Integer

numParams :: Opcode -> Integer
numParams Add = 3
numParams Mul = 3
numParams Input = 1
numParams Output = 1
numParams JumpIfTrue = 2
numParams JumpIfFalse = 2
numParams LessThan = 3
numParams Equals = 3
numParams AdjustBase = 1
numParams Terminate = 0

data Machine = Machine
  { _counter :: Counter,
    _inputs :: [Integer],
    _outputs :: [Integer],
    _program :: Program,
    _relBase :: Integer
  }
  deriving (Show)

makeLenses ''Machine

unsafeMemAccess (Just a) = a
unsafeMemAccess Nothing = 0

runInstruction :: Opcode -> [Mode] -> [Param] -> Machine -> Machine
runInstruction opcode modes params machine =
  case opcode of
    Terminate -> machine
    Add ->
      machine & program %~ M.insert writebackLocation (head paramVals + (paramVals !! 1))
        & counter %~ (+ 4)
    Mul ->
      machine & program %~ M.insert writebackLocation (head paramVals * (paramVals !! 1))
        & counter %~ (+ 4)
    Input ->
      machine & program %~ M.insert writebackLocation (head $ machine ^. inputs)
        & inputs %~ tail
        & counter %~ (+ 2)
    Output ->
      machine & outputs %~ (++ [head paramVals])
        & counter %~ (+ 2)
    JumpIfTrue -> case head paramVals of
      0 -> machine & counter %~ (+ 3)
      _ -> machine & counter .~ (paramVals !! 1)
    JumpIfFalse -> case head paramVals of
      0 -> machine & counter .~ (paramVals !! 1)
      _ -> machine & counter %~ (+ 3)
    LessThan ->
      if head paramVals < paramVals !! 1
        then
          machine & program %~ M.insert writebackLocation 1
            & counter %~ (+ 4)
        else
          machine & program %~ M.insert writebackLocation 0
            & counter %~ (+ 4)
    Equals ->
      if head paramVals == paramVals !! 1
        then
          machine & program %~ M.insert writebackLocation 1
            & counter %~ (+ 4)
        else
          machine & program %~ M.insert writebackLocation 0
            & counter %~ (+ 4)
    AdjustBase ->
      machine & relBase %~ (+ head paramVals)
        & counter %~ (+ 2)
  where
    paramVal (param, mode) = case mode of
      Immediate -> param
      Positional -> unsafeMemAccess $ M.lookup param (machine ^. program)
      Relative -> unsafeMemAccess $ M.lookup (param + (machine ^. relBase)) (machine ^. program)
    paramVals = paramVal <$> zip params modes
    writebackLocation = if last modes == Relative then last params + (machine ^. relBase) else last params

zeroPadTo :: Int -> String -> String
zeroPadTo l x = replicate (l - length x) '0' ++ x

toMode :: Char -> Mode
toMode '0' = Positional
toMode '1' = Immediate
toMode '2' = Relative
toMode e = error $ "Invalid mode: " ++ [e]

opFromStr :: String -> Opcode
opFromStr "01" = Add
opFromStr "02" = Mul
opFromStr "03" = Input
opFromStr "04" = Output
opFromStr "05" = JumpIfTrue
opFromStr "06" = JumpIfFalse
opFromStr "07" = LessThan
opFromStr "08" = Equals
opFromStr "09" = AdjustBase
opFromStr "99" = Terminate
opFromStr s = error s

parseOpcode :: Integer -> (Opcode, [Mode])
parseOpcode x =
  ( opcode,
    toMode
      <$> reverse
        ( take (fromIntegral $ numParams opcode) $
            zeroPadTo (fromIntegral $ numParams opcode + 2) opStr
        )
  )
  where
    opStr = show x
    opcode = opFromStr $ zeroPadTo 2 $ drop (length opStr - 2) opStr

getCurrentOp :: Machine -> (Opcode, [Mode])
getCurrentOp machine = parseOpcode $ unsafeMemAccess $ M.lookup (machine ^. counter) (machine ^. program)

stepProgram :: Machine -> IO Machine
stepProgram machine =
  {-
  do
    putStrLn $ "Counter: " ++ show (machine ^. counter)
    putStrLn $ "RelBase: " ++ show (machine ^. relBase)
    putStrLn $ "Op/Params/Modes:" ++ show opcode ++ show params ++ show modes
    putStrLn $ "In/Out:" ++ show (machine ^. inputs) ++ show (machine ^. outputs)
    print (machine ^. program)
    getLine
  -}
  return $ runInstruction opcode modes params machine
  where
    (opcode, modes) = getCurrentOp machine
    params =
      catMaybes $
        M.lookup
          <$> [(machine ^. counter) + 1 .. (machine ^. counter) + numParams opcode]
          <*> [machine ^. program]

runProgram :: Machine -> IO Machine
runProgram machine =
  if isTerminated machine
    then pure machine
    else runProgram =<< stepProgram machine

-- Whether or not the machine is currently blocked from running
isBlocked :: Machine -> Bool
isBlocked machine = isTerminated machine || (opcode == Input && null (machine ^. inputs))
  where
    (opcode, _) = getCurrentOp machine

isTerminated :: Machine -> Bool
isTerminated machine = opcode == Terminate
  where
    (opcode, _) = getCurrentOp machine

-- Proceed until we either have N outputs, or the machine terminates.
stepUntilNOutputs :: Int -> Machine -> IO Machine
stepUntilNOutputs n machine =
  if isTerminated machine || length (machine ^. outputs) == n
    then return machine
    else stepUntilNOutputs n =<< stepProgram machine

-- Proceed until we either have N inputs, or the machine terminates.
stepUntilNInputs :: Int -> Machine -> IO Machine
stepUntilNInputs n machine =
  if isTerminated machine || length (machine ^. inputs) == n
    then return machine
    else stepUntilNInputs n =<< stepProgram machine

readProgram :: String -> IO Program
readProgram path = do
  program <- fmap read . splitOn "," . head . lines <$> readFile path
  return $ M.fromList $ zip [0 .. toInteger (length program - 1)] program
