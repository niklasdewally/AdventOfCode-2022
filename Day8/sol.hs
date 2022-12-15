import Data.Char
import Data.List

-- Parse the input into a [row,column] matrix
parseInput :: String -> [[Int]]
parseInput = (map (map digitToInt)) . lines


{- Approach: do this forward, backward, and by column, 
    and OR them together 
    
    This only marks visible from L->R
-}

markVisibleInRow :: [Int] -> [Bool]
markVisibleInRow (x:xs) = True:(inner xs x)
    where inner (x:[]) lastHighest = [True]
          inner (x:xs) lastHighest = if x > lastHighest 
                                        then True:(inner xs x)
                                        else False:(inner xs lastHighest)
getViewDistanceL :: [Int] -> [Int]
getViewDistanceL (x:[]) = [0]
getViewDistanceL (x:xs) = (inner xs x 0):(getViewDistanceL xs)
    where inner (x:xs) height i = if x < height
                                     then inner xs height (i+1)  -- continue
                                     else (i+1)      -- stop
                                     -- note that it can see the tree that blocks it!
          inner [] height i = i

markVisible grid = 
    let lToR = map markVisibleInRow grid
        rToL = map (reverse.markVisibleInRow.reverse) grid
        uToD = transpose $ map (markVisibleInRow) $ transpose grid
        dToU = transpose $ map (reverse.markVisibleInRow.reverse) $ transpose grid
     in foldl (\g1 g2 -> zipWith (\x y -> x || y ) g1 g2) (concat lToR) (map concat [lToR,rToL,uToD,dToU])

getViewDistance grid = 
    let lToR = map getViewDistanceL grid
        rToL = map (reverse.getViewDistanceL.reverse) grid
        uToD = transpose $ map (getViewDistanceL) $ transpose grid
        dToU = transpose $ map (reverse.getViewDistanceL.reverse) $ transpose grid
     in foldl (\g1 g2 -> zipWith (\x y -> x * y ) g1 g2) (concat lToR) (map concat [rToL,uToD,dToU])
-- [[[Int]]] -> [[Int]] -> [Int] -> [[Int]]
--
--      CONCAT        FOLD , ZIP   SPLITAT

main :: IO ()
main = do
    content <- getContents
    let grid = parseInput content

    print $ length $ filter (\x -> x) $ markVisible grid
    print $ maximum $ getViewDistance grid
