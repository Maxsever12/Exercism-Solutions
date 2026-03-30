module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSum(n) - sumOfSquares(n)

squareOfSum :: Integral a => a -> a
squareOfSum 0 = 0
squareOfSum n = addition n * addition n

addition :: Integral a => a -> a
addition 0 = 0
addition n = addition (n - 1) + n

sumOfSquares :: Integral a => a -> a
sumOfSquares 0 = 0
sumOfSquares n = sumOfSquares(n - 1) + (n * n)
