

import Data.List (sort)


inverseBWT :: [String] -> String
inverseBWT lst = inverseBWT1 lst lst


inverseBWT1:: [String] -> [String] -> String
inverseBWT1 lst1 lst
 | length lst1 == length (lst !! 0)= originalString lst
 | otherwise = inverseBWT1 lst1 (concatList lst1 (sort lst))

originalString lst 
	|last (lst !! 0) == '$' = lst !! 0
	|otherwise = originalString (tail lst)

concatList:: [String] -> [String] -> [String]
concatList [] [] = []
concatList lst1@(x1:xs1) lst2@(x2:xs2) = [x1 ++ x2] ++ concatList xs1 xs2


-- give inverseBWT an input of ["e", "$", "e", "l", "p", "l", "e", "p", "a"]