{-
-  Lempel Ziv Welch decompression
-  Hammad Siddiqui
-}

import Data.List (sort)
import Data.List.Split
import Data.Char
import System.IO

decompressedStr = do
	        compressedStr <- readFile fname
		    writeFile outname (splitOn " " compressedStr)

--lzwInv :: String -> String
--lzwInv compressedStr =

-- Tokenize using space as delimiter
-- splitOn " " compressedStr


-- read code and write it's corosponding char(s)