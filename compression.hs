{-
- Piep Piper's Middle out compression
- Using Burrows-Wheeler transformation and Lempel Ziv Welch compression scheme
- Collaborators: Hammad Siddiqui & Sumbul Zehra
-}



import Data.List
import Data.List (sort)
import Data.List.Split
import System.IO   
import Data.Char
--------------------- Custom Types -------------------------------

type Dict = [(Int , String, Int)] -- Custom type to store dicitonary
type BWT = [String]  -- Custom type for BWT matrix
   
--------------- Main Functions to run on terminal ----------------
-- Compression with BWT Transform
compressionBWT fname outname = do  
            contents <- readFile fname
            writeFile outname (mergeOut (compress ([head (genBWTmatrix contents)]) (tail (genBWTmatrix contents)) []))
		 
-- Compression without BWT Transform
compressionNoBWT fname outname = do  
            contents <- readFile fname
            writeFile outname (mergeOut (compress ([head  contents]) (tail contents) []))
		  
-- Decompression with BWT Transform
decompressionBWT fname outname = do  
            compressedStr <- readFile fname
            writeFile outname (inverseBWT (decompress (splitOn " " compressedStr) []))

-- decompression without BWT Transform
decompressionNoBWT fname outname = do  
            compressedStr <- readFile fname
            writeFile outname (decompress (splitOn " " compressedStr) [])

   
----------------------Compression-------------------------------

--returns the first, second and third elements of the tuple respectively
ft (a, _, _) = a
sd (_ , a, _) = a
thd (_, _, a) = a


--Takes a string(key) and a dictionary and returns its value.
findStr :: String -> Dict -> Int
findStr str lst
    |lst == [] = -1
    |sd (head lst) == str = thd (head lst)
    |otherwise = findStr str (tail lst)


--Takes a string(key) and a dictionary and returns its value. The value is its Ascii code if its a character
value :: String -> Dict-> Int
value str d
    |length str == 1 = ord (head str)
    |otherwise = findStr str d

--adds (output ,key, value) to the dictionary. Called when a new string is added to the dictioanry.  
add :: String -> String -> Dict -> Dict
add oldstr newstr d  = d ++ [( value oldstr d , newstr, (maxIndex d) +1)]

-- calculates the next value for insertion.
maxIndex d = 255 + (length d)

--performs the Lempel Ziv Welch compression algorithm
compress :: String -> String -> Dict -> Dict
compress current str d
    |str == "" =  d
    |(findStr (current ++ [str!!0]) d) /= -1 = compress (current ++ [str !! 0]) (tail str) d --(current + next) already present in d. current = current + next
    |otherwise = compress ([str !! 0]) (tail str) (add current (current ++ [str !! 0]) d) -- add (current + next) to d. save the value of current for output 

--extract and merge all the ouput values from d, separated by spaces
mergeOut :: Dict -> String
mergeOut lst
    |lst == [] = ""
    |otherwise = show (ft (head lst)) ++ " " ++ mergeOut (tail lst) 


------------------ Decompress -----------------

{-
- decompress recieves a list of integers of type string.
- for e.g: ["97","98","258","123"]
- starting from the first element. It converts string to int: "97" -> 97
- if the element is under 255, the 'chr' function prints its corrosponding ASCII letter
- Concatenation of the current letter with the next letter is added to the Dict with a value > 255
- Decompress is then recursively called on the tail of the list
-}

decompress :: [String] -> Dict -> String
decompress [] d = "" 
decompress [""] d = "" 
decompress lst@(x:xs) d = if (read x :: Int) <= 255  then [chr (read x :: Int)] ++ (decompress xs (add2 [chr (read (x) :: Int)] [chr (read (head xs) :: Int)] d))
    else (findInt (read x :: Int) (add2 x (head xs) d)) ++ (decompress xs (add2 [chr (read (x) :: Int)] [chr (read (head xs) :: Int)] d))


-- Dictionary addition for decompress
add2 :: String -> String  -> Dict -> Dict
add2 str1 str2 d = d ++ [(0, (str1++str2), (maxIndex d) + 1)] 


-- Finds the string corrosponding to a value greater then 255 in the dictionary
findInt :: Int -> Dict -> String
findInt i [] = [chr i]
findInt i d@(x:xs) 
    | i <= 255 = [chr i]
    | thd x == i = sd x
    | thd x /= i = findInt i xs
    | otherwise = ""


--------------------- BWT -----------------------
--converts a string of the form "cats" to ["c" , "a", "t" , "s"]	   
listChar :: String -> [String]
listChar str
    |length str == 1 = [str] 
    |otherwise = [[head str]] ++ listChar (tail str) 

-- takes a string and performs inverse BWT tranform on it
inverseBWT :: String -> String
inverseBWT str = inverseBWT2 (listChar str)

--helper function for inverseBWT
inverseBWT2 :: [String] -> String
inverseBWT2 lst = inverseBWT1 lst lst

--helper function for inverseBWT
inverseBWT1:: [String] -> [String] -> String
inverseBWT1 lst1 lst
    | length lst1 == length (lst !! 0)= init (originalString lst)
    | otherwise = inverseBWT1 lst1 (concatList lst1 (sort lst))

--returns the string that ends with '$'
originalString :: [String] -> String
originalString lst 
    |last (lst !! 0) == '$' = lst !! 0
    |otherwise = originalString (tail lst)

-- if list1  = ["c" , "a", "t" , "s"] , list2 =  ["b" , "a", "t" , "s"] : concatList =  ["cb" , "aa", "tt" , "ss"]
-- Run on same length lists
concatList:: [String] -> [String] -> [String]
concatList [] [] = []
concatList lst1@(x1:xs1) lst2@(x2:xs2) = [x1 ++ x2] ++ concatList xs1 xs2


genBWTmatrix :: String -> String
genBWTmatrix str = last (transpose (sort (rotateBWT [str ++ "$"])))

rotateBWT :: BWT -> BWT
rotateBWT a = if (head l) == '$' then a
              else a ++ rotateBWT n
                where l = last a
                      n = [(tail l) ++ [head l]]



----------------------------- Dr. Waqar's Strategy for BWT ------------------
someString = ["Banana"]
genBWTmatrix2 = (transpose (sort(transpose (rotateBWT2 [(head someString) ++ "$"])))) !! 1

rotateBWT2 :: BWT -> BWT
rotateBWT2 a = a ++ [[last (head a)] ++ init (head a)]