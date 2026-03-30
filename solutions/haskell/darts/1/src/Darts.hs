module Darts (score) where

distance :: Float -> Float -> Float
distance x y = sqrt(x * x + y * y)

score :: Float -> Float -> Int
score x y | distance x y > 10 = 0
          | distance x y > 5 = 1
          | distance x y > 1 = 5
          | True = 10


