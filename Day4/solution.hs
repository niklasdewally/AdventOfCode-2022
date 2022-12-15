import Data.List
import Data.Ix
parseInput :: [String] -> [[[Int]]]
parseInput inputLines =
    map parseLine inputLines
        where parseLine line = map parseElf (splitAt ',' line)
              parseElf elf = map (\x -> read x :: Int) (splitAt '-' elf)

              splitAt :: Char -> String -> [String]
              splitAt char xs = reverse $ splitAtIter char xs "" []

              splitAtIter :: Char -> String -> String -> [String] -> [String]
              splitAtIter char lst currentSegment segments
                | length lst == 0 = (reverse currentSegment) : (segments) -- terminate
                | (head lst) == char = splitAtIter char (tail lst) "" ((reverse currentSegment) : segments) -- new segment
                | otherwise = splitAtIter char (tail lst) ((head lst) : currentSegment) segments -- recur


checkPair :: ([Int] -> [Int] -> Bool) -> [[Int]] -> Bool
checkPair predicate xs = predicate first second
    where first = (xs!!0)
          second = (xs!!1)

part1 :: [[[Int]]] -> [[[Int]]]
part1 = filter (checkPair predicate)
    where predicate :: [Int] -> [Int] -> Bool
          predicate (x1:x2:xs) (y1:y2:ys) = (x1 <= y1 && x2 >= y2) || (y1 <= x1 && y2 >= x2)
          predicate _ _ = False


part2 :: [[[Int]]] -> [[[Int]]]
part2 = filter (checkPair predicate)
    where predicate :: [Int] -> [Int] -> Bool
          predicate (x1:x2:[]) (y1:y2:[]) = intersect (range (x1,x2)) (range (y1,y2)) /= []
          predicate _ _ = False

main :: IO ()
main = do 
    contents <- getContents 
    let pairs = parseInput (lines contents)
        part1Pairs = part1 pairs
        part2Pairs = part2 pairs
        sol1 = length part1Pairs
        sol2 = length part2Pairs

    print sol1
    print sol2
