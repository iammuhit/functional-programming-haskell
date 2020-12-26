module SumSquareDifference(sumSquareDiff) where

{-  squaresSum n

    TAKES:    The highest range
    RETURNS:  Computes the sum of squares of numbers from 0 to n
    EXAMPLES: squaresSum 10 == 385
    VARIANT:  n
-}
squaresSum :: Integer -> Integer
squaresSum n = 
  if n == 0 || n == 1 then 
      n
  else 
      if n < 0 then 
          (n * n) + squaresSum (n + 1)
      else 
          (n * n) + squaresSum (n - 1)

{-  numbersSum n

    TAKES:    The highest range
    RETURNS:  Computes the sum of numbers from 0 to n
    EXAMPLES: numbersSum 10 == 55
    VARIANT:  n
-}
numbersSum :: Integer -> Integer
numbersSum n = 
  if n == 0 || n == 1 then 
      n
  else 
      if n < 0 then 
          n + numbersSum (n + 1)
      else 
          n + numbersSum (n - 1)

{-  sumSquareDiff n

    TAKES:    The highest range
    RETURNS:  The difference between the square of the sum and the sum of the squares of all natural numbers from 1 to n
    EXAMPLES: sumSquareDiff 10 == 2640
-}
sumSquareDiff :: Integer -> Integer
sumSquareDiff n = ((numbersSum n) ^ 2) - (squaresSum n)