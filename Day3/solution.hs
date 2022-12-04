import Data.Char
import Data.List
import Data.Maybe

part1 :: String -> Maybe Char
part1 str =
  (\x -> part1' (fst x) (snd x)) $
  (\(x, y) -> ((sort x), (sort y))) $ splitAt ((length str) `div` 2) str
 -- conceptually similar to merge step of merge sort
  where
    part1' :: String -> String -> Maybe Char
    part1' (x:xs) (y:ys)
      | x == y = Just x
      | x < y = part1' xs (y : ys)
      | otherwise = part1' (x : xs) ys
    part1' xs [] = Nothing
    part1' [] ys = Nothing

letterToNum :: Char -> Int
letterToNum char
  | isLower char = (ord char) - (ord 'a') + 1
  | isUpper char = (ord char) - (ord 'A') + 27

groupInThrees :: [String] -> [[String]]
groupInThrees inputLines = inner inputLines [] [] 0
  where
    inner xs output currentGroup lineNumber
      | xs == [] =
        if newGroup
          then init (currentGroup : output)
          else init output -- exhausted input. Note we must get rid of the [] at the end
      | newGroup =
        inner (tail xs) (currentGroup : output) [head xs] (lineNumber + 1) -- new group
      | otherwise =
        inner (tail xs) output ((head xs) : currentGroup) (lineNumber + 1) -- recursive case
      where
        newGroup = lineNumber `mod` 3 == 0

part2 :: [String] -> Char
part2 elves =
  (\x -> (x !! 0)) $
  (\x -> findDups (x !! 0) (findDups (x !! 1) (x !! 2))) $ map sort elves
  -- do same as part1, but return a list of duplicates
  where
    findDups :: String -> String -> [Char]
    findDups (x:xs) (y:ys)
      | x == y = x : (findDups xs ys)
      | x < y = findDups xs (y : ys)
      | otherwise = findDups (x : xs) ys
    findDups x [] = ""
    findDups [] y = ""

main :: IO ()
main = do
  content <- getContents
  let inpLines = lines content
      score1 = sum (map (letterToNum . fromJust . part1) inpLines)
      score2 = sum $ map (letterToNum . part2) $ groupInThrees inpLines
  print score1
  print score2
