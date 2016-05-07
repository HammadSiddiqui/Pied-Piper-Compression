{-
-	Burrrows_Wheeler Transform
- 	Pied Piper
-}

-- type String = [Char]

import Data.List (sort) 
import System.IO 

type BWT = [String]
samplebwt = ["BANANA$"]


genBWTmatrix :: String -> BWT
genBWTmatrix str = [last (transpose (sort (rotateBWT [str ++ "$"])))]

rotateBWT :: BWT -> BWT
rotateBWT a = if (head l) == '$' then a
              else a ++ rotateBWT n
                where l = last a
                      n = [(tail l) ++ [head l]]

 
main = mapM_ putStrLn $ genBWTmatrix "Anas"

----------------------------- Dr. Waqar's Strategy ------------------
someString = ["Anas"]
genBWTmatrix2 = (transpose (sort(transpose (rotateBWT2 [(head someString) ++ "$"])))) !! 1

rotateBWT2 :: BWT -> BWT
rotateBWT2 a = a ++ [[last (head a)] ++ init (head a)]