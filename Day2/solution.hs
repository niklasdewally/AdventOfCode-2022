-- Define some magic numbers
draw = 3
win = 6
loss =  0


-- win if yourMove = opponentsMove +1 mod 3
-- draw if yourMove = opponentsMove
-- lose if yourMove = opponentsMove -1 mod 3

part1 :: Int -> Int -> Int
part1 opponentsMove yourMove
    | yourMove == opponentsMove = yourMove + draw + 1
    | yourMove == (opponentsMove + 1) `mod` 3 = yourMove + win + 1
    | otherwise = yourMove + loss + 1

lettersToNums :: String -> Int
lettersToNums "A" = 0
lettersToNums "B" = 1
lettersToNums "C" = 2
lettersToNums "X" = 0
lettersToNums "Y" = 1
lettersToNums "Z" = 2

lettersToNums2 :: String -> Int
lettersToNums2 "A" = 0
lettersToNums2 "B" = 1
lettersToNums2 "C" = 2
lettersToNums2 "X" = loss
lettersToNums2 "Y" = draw
lettersToNums2 "Z" = win

part2 :: Int -> Int -> Int
part2 opponentsMove outcome = outcome + getMove + 1 
    where getMove
           | outcome == draw = opponentsMove
           | outcome == win = (opponentsMove + 1) `mod` 3
           | otherwise = (opponentsMove - 1) `mod` 3

main :: IO()
main = do
    contents <- getContents

    let wordedContent = map words $ lines contents
        score1 = sum $ map ((\x -> part1 (x!!0) (x!!1)) . (map lettersToNums)) wordedContent 
        score2 = sum $ map ((\x -> part2 (x!!0) (x!!1)) . (map lettersToNums2)) wordedContent

    print score1
    print score2 
