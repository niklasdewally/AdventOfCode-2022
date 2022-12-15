import Data.Maybe
import Data.List


isStartPacket lst = (nub lst) == lst && (length lst) == 4
isStartMessage lst = (nub lst) == lst && (length lst) == 14

part1 :: [Char] -> Int

part1 xs = fromJust $ part1Iter [] xs 0
    where
        part1Iter slidingWindow [] i = Nothing

        part1Iter slidingWindow rest i 
            | isStartPacket slidingWindow = Just i
            | otherwise = part1Iter slidingWindow' rest' i'
          where 
              curr = head rest
              slidingWindow' = curr:(take 3 slidingWindow)
              i' = i + 1
              rest' = tail rest

part2 :: [Char] -> Int

part2 xs = fromJust $ part1Iter [] xs 0
    where
        part1Iter slidingWindow [] i = Nothing

        part1Iter slidingWindow rest i 
            | isStartMessage slidingWindow = Just i
            | otherwise = part1Iter slidingWindow' rest' i'
          where 
              curr = head rest
              slidingWindow' = curr:(take 13 slidingWindow)
              i' = i + 1
              rest' = tail rest

main :: IO()
main = do
    contents <- getContents
    print $ part1 contents
    print $ part2 contents
