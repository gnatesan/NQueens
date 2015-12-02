module NQueens where 
import Data.List

{--
Given a size parameter (n) this function returns a list of lists. Each inner list
represents a unique permutation of a list from 1 to n. These values indicate
the position of a queen on the game board where the value is the row and the index
of that value in its respective list is the column. This ensures that no 2 queens will
be in the same row or column. 
Ex: solnPermuations 3 = [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]] 
--}
solnPermutations :: Int -> [[Int]]
solnPermutations 0 = [[]]
solnPermutations n = permutations [1..n]
 
{--
Given an integer and list of integers as parameters, where the list is a unique permutation
and the integer is the first element from that unique permutation, this function evaluates
the positions of the queens to return a boolean value on whether two queens are on the same 
diagonal.
--}
testDiag :: Int -> [Int] -> Bool
testDiag a bc = any (\(col, row) -> abs (row - a) == col) $ zip [1..] bc
 
--This function takes in a unique permutation and returns whether if it is a valid alignment of queens
testPermutation :: [Int] -> Bool
testPermutation [] = True
testPermutation (a:bc) = not (testDiag a bc) && testPermutation bc

{--
This function takes a list of inner lists containing all possible queen configurations and tests to see
if it is an actual solution.
--}

allSolns :: [[Int]] -> [[Int]]
allSolns [[]] = [[]]
allSolns a = filter testPermutation a

--This function takes in a size parameter and returns how many different possible solutions there are.
nQueensNumSoln :: Int -> Int
nQueensNumSoln n = 
    let allLists = solnPermutations n
    in length $ allSolns allLists  

--This function takes in a size parameter and will print out all valid board configurations.
nQueensSolutions :: Int -> [[Int]]
nQueensSolutions n =
    let allLists = solnPermutations n
    in allSolns allLists
{--
This function takes in a list of integers representing a valid positioning of queens and will
print the corresponding alignment as a board.
--}

-- list from 1 to number of queens -> solution -> character printout
generateBoard :: [Int] -> [Int] -> String
generateBoard [] _ = ""
generateBoard (x:xs) (y) = generateLine x y ++ "\n" ++ generateBoard xs y


-- row -> solution -> line printout
generateLine :: Int -> [Int] -> String
generateLine a b =
    case elemIndex a b of
      Just val -> test (val - 1) "X" ++ test 0 "Q"  ++ test (length b - val - 2) "X"
      Nothing -> "invalid"
    --let val = elemIndex a b --val contains the column where 'Q' should be printed in the specified row
    --in test (val-1) "x"
    
-- takes in an int and string and repeats the string that many times
test :: Int -> String -> String
test k s = concat [s | r <- [0..k]]

printAllSolns :: Int -> [[Int]] -> [[Char]]
printAllSolns n = map (generateBoard [1..n])

nQueensText :: Int -> [[Char]] 
nQueensText n = printAllSolns n (nQueensSolutions n)

nQueens n 
	| (nQueensNumSoln n) > 0 = mapM_ putStrLn (nQueensText n)
	| otherwise = putStrLn "There is No Solution"
    






    
