import Control.Monad
import Data.List

part1 :: [[Int]] -> Int
part1 = foldr max (-1) . (map sum)

part2 :: [[Int]] -> Int
part2 =  sum . (take 3) . reverse . sort . (map sum)

parseInput :: [String] -> [[Int]]
parseInput a = reverse $ parseInputIter a [] []

parseInputIter :: [String] -> [Int] -> [[Int]] -> [[Int]]

parseInputIter (x:xs) currentList listOfLists
    | xs == [] && x == "" = (reverse currentList):listOfLists
    | xs == [] = ((read x::Int):currentList):listOfLists
    | x  == "" = parseInputIter xs [] ((reverse currentList):listOfLists)
    | otherwise = parseInputIter xs ((read x::Int):currentList) listOfLists

main :: IO()
main = do
       contents <- getContents
       let parsedInput = parseInput $ lines contents
       print $ part1 parsedInput
       print $ part2 parsedInput
