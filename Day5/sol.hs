import Text.Parsec.String
import Text.Parsec
import Data.Either
import Data.Maybe

--- AST

type Instruction = (Int,Int,Int)
type StackRow = String
data AST = AST [StackRow] [Instruction] deriving (Show)

instruction :: String -> String -> String -> Instruction
instruction a b c  = ((read a),(read b),(read c))

toStackRows :: AST -> [StackRow]
toStackRows (AST s _) = s

toInstructions :: AST -> [Instruction]
toInstructions (AST _ i) = i

--- PARSER

inputFile :: Parser AST
inputFile =
    do 
       stack <- many1 stackRows
       instructions <- many1 instructions
       return (AST stack instructions)

stackRows = do 
    cs <- many1 (try (emptyStackFrame <|> stackFrame))
    endOfLine
    return cs

stackFrame = 
    do optional (string " ")
       string "["
       c <- letter 
       string "]"
       optional (string " ")
       return c

emptyStackFrame = 
    do count 3 (string " ")
       optional (string " ")
       return '_'

instructions = 
    do spaces
       string "move"
       spaces
       n <- (many1 digit)
       spaces
       string "from"
       spaces
       src <- (many1 digit)
       spaces
       string "to"
       spaces
       dest <- (many1 digit)
       spaces
       optional (char '\n')
       return (instruction n src dest)

--- STACK DATA TYPE

type Stack = [Char]

pop :: Stack -> (Stack,Maybe Char)
pop [] = ([],Nothing)
pop (x:xs) = (xs,Just x)


push :: Stack -> Char -> Stack
push stack char = char:stack

--- SOLUTION

part1 :: AST -> Int

part1 (AST stackRows instructions) = 
    let initialStacks = buildStack stackRows
        endStacks = fst (iterate (\(s,ins) -> ((runInstruction s (head ins)),tail ins)) (initialStacks,instructions)) !!(length instructions)
     in sum (map (\s -> (toJust $ snd $ pop s)) endStack)

buildStack :: [StackRow] -> [Stack]
buildStack [] = []
buildStack rows = buildStackI rows (replicate (length $ head rows) [])
    where
        buildStackI :: [StackRow] -> [Stack] -> [Stack]
        buildStackI _ [] = []
        buildStackI [] currStacks = currStacks
        buildStackI (row:rows) currStacks = buildStackI rows newStacks
            where newStacks = map (\(i,s)-> if (row!!i) /= '_' then push s (row!!i) else s) (zip [0..] currStacks)

runInstruction :: [Stack] -> Instruction -> [Stack]
runInstruction stacks (n,src,dest) = 
    let srcStack = (stacks!!src)
        destStack = (stacks!!dest)
        res =  (iterate (\(s,d) -> 
            let popResult = pop s
                val = fromJust (snd popResult)
                s'  = fst popResult
                d' = push d val
             in (s',d')) (srcStack,destStack) )!!n

        newStacks = (take src stacks) ++ [fst res] ++ (take dest (drop (src+1) stacks)) ++ [snd res] ++ (drop (dest+1) stacks)
     in newStacks

--- MAIN
main = do
    contents <- getContents
    let ast = fromRight (AST [] []) (parse inputFile "" contents)
    print $ buildStack (toStackRows ast)
