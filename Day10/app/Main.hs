module Main where
import Data.List.Split
import Data.Maybe
import Data.List
import Text.Parsec hiding (State)
import Text.Parsec.Number
import Data.Either
import Control.Monad
import Control.Monad.State
import Data.Function


-- The state of a cpu at a cycle C is (C,register X)
type CpuState = (Int,Int)
 
-- A Monad to encapsulate state changes of the cpu
-- State stored as an array so that history can be kept
type Machine = State [CpuState] ()

-- An instruction to be ran on a machine
type Instruction =  CpuState -> CpuState

-- An idempotent machine. This machine has a single instruction, that does no mutation of state.
idMachine = (modify (\x -> x))

-- Add an instruction to a machine, to be executed last
addInstruction :: (CpuState -> CpuState) -> Machine -> Machine
addInstruction instruction machine = machine >> modify (\(s:ss) -> (instruction s):s:ss)


-- Convert a list of instructions to a machine that runs the instructions in order.
toMachine :: [Instruction] -> Machine
toMachine instructions = 
    foldl (\machine instruction -> addInstruction instruction machine) idMachine instructions


-- Run a machine for a given initial value of X, returning the list of all states of the machine
runMachine :: Machine -> Int -> [CpuState]
runMachine m x0 = execState m [(0,x0)] 
                    & reverse

-- Instructions
noop :: Instruction
noop (cycle,x) = (cycle + 1,x)

addx :: Int -> Instruction
addx n (cycle,x) = (cycle + 2,x + n)

-- Parsing
parseInp = do
    a <- many1 $ do cmd <- pAddx <|> pNoop
                    return cmd
    return a

pAddx = do
    string "addx" 
    spaces
    a <- int
    spaces
    return $ addx a

pNoop = do
    string "noop"
    spaces
    return $ noop

findXatCycle (-1) _ = Nothing
findXatCycle cycle states  =
    -- "during" the nth cycle, not after. so find cycle -1
    let res = find (\(c,x) -> (cycle -1)== c) states
        in case res of
             Nothing -> findXatCycle (cycle - 1) states
             Just a -> Just ((snd a))

signalStrength (-1) _ _ = Nothing
signalStrength cycle cycle0 states  =
    let res = find (\(c,x) -> (cycle -1)== c) states
    -- "during" the nth cycle, not after. so find cycle -1
        in case res of
             Nothing -> signalStrength (cycle - 1) cycle0 states
             Just a -> Just (cycle0 * (snd a))

part1 :: [CpuState] -> Maybe Int
part1 states = do
    a <- signalStrength 20 20 states
    b <- signalStrength 60 60 states
    c <- signalStrength 100 100 states
    d <- signalStrength 140 140 states
    e <- signalStrength 180 180 states
    f <- signalStrength 220 220 states

    return $ a+b+c+d+e+f

-- Returns a list of marked pixels using the given machine-states
markPixels :: [CpuState] -> String
markPixels states = markPixels' 0 states

markPixels':: Int -> [CpuState] -> String
markPixels' 240 states = 
    let middle = case (findXatCycle 241 states) of
                   Nothing -> error "Expected to find value at cycle 241, got none"
                   Just a -> a
        x = 240 `mod` 40
        mark = if middle == x || (middle + 1 ) == x || (middle -1) == x then '#' else '.'
     in [mark]

markPixels' i states = 
    let middle = case (findXatCycle (i+1) states) of 
                   Nothing -> error $ "Expected to find value at cycle " ++ (show (i+1)) ++  " , got none"
                   Just a -> a
        x = i `mod` 40
        mark = if (middle == x || (middle + 1) == x || (middle - 1) == x) then '#' else '.'
     in mark:(markPixels' (i+1) states)

-- loop from 0-39, and everytime we reach 0, add a newline to the string
printCRT :: String -> String
printCRT str = printCRT' str 0

printCRT' [] _ = []
printCRT' (x:xs) i = 
    let i' = (i+1) `mod` 40
        output = if i==0 then '\n':[x] else [x]
     in output ++ (printCRT' xs i')


main = do
    contents <- getContents
    let instructions = parse parseInp "" contents
        machine = case instructions of 
                    Right ins -> toMachine ins
                    Left a -> error $ show a
        states = runMachine machine 1

    print $ states
    print $ part1 states
    putStrLn $ printCRT $ markPixels states
