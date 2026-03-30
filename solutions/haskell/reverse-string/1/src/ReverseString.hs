module ReverseString (reverseString) where

reverseString :: String -> String
reverseString [] = []
reverseString (a:as) = reverseString as ++ [a]
