module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum = [(x,y,z) | x <- [1..sum], y <- [x..sum - x], let z = sum - x - y, (x^2) + (y^2) == (z^2)]
