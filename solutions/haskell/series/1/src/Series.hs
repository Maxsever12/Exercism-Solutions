module Series (slices) where

slices :: Int -> String -> [[Int]]
slices 0 a = replicate (length a + 1) []
slices n [] = []
slices n (c:cs) | n > length(cs) + 1 = []
                | otherwise = ([toInt c] ++ map toInt (take (n-1) cs)) : slices n cs

toInt :: Char -> Int
toInt c = read [c]