import Data.List

generateSolutions' :: Int -> [[Int]]
generateSolutions' 0 = [[]]
generateSolutions' n = permutations [1..n]
 
duplicateDiagonals :: Int -> [Int] -> Bool
duplicateDiagonals q qs = any (\(columnDistance, row) -> abs (row - q) == columnDistance) $ zip [1..] qs
 
test :: [Int] -> Bool
test [] = True
test (q:qs) = not (duplicateDiagonals q qs) &&test qs





















