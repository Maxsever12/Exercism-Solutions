module Scrabble (scoreLetter, scoreWord) where

scoreLetter :: Char -> Integer
scoreLetter letter
            | elem letter "QZqz" = 10
            | elem letter "JXjx" = 8
            | elem letter "Kk" = 5
            | elem letter "FHVWYfhvwy" = 4
            | elem letter "BCMPbcmp" = 3
            | elem letter "DGdg" = 2
            | elem letter "aeioulnrstAEIOULNRST" = 1
            | otherwise = 0

scoreWord :: String -> Integer
scoreWord [] = 0
scoreWord (a:as) = scoreLetter(a) + scoreWord(as)
