{-
    add m n

    @takes      Two integer numbers
    @returns    The addition of the two numbers
    @examples   add 1 2 == 3
                add 4 5 == 9
-}
add :: Integer -> Integer -> Integer
add = \m n -> m + n

{-
    minus m n

    @takes      Two integer numbers
    @returns    The subtraction of the two numbers
    @examples   minus 2 1 == 1
                minus 8 5 == 3
-}
minus :: Integer -> Integer -> Integer
minus = \m n -> m - n

{-
    multiply m n

    @takes      Two integer numbers
    @returns    The multiplication of the two numbers
    @examples   multiply 2 1 == 2
                multiply 3 2 == 6
-}
multiply :: Integer -> Integer -> Integer
multiply = \m n -> m * n

{-
    divide m n

    @takes      Two numbers
    @returns    The division of the two numbers
    @examples   divide 4 2 == 2.0
                divide 6 4 == 1.5
-}
divide :: Double -> Double -> Double
divide = \m n -> m / n

{-
    divide' m n

    @takes      Two integer numbers
    @returns    The division of the two numbers, but rounded result
    @examples   divide' 4 2 == 2
                divide' 6 4 == 1
-}
divide' :: Integer -> Integer -> Integer
divide' = \m n -> m `div` n

{-
    modulus m n

    @takes      Two integer numbers
    @returns    The modulus of the two numbers
    @examples   modulus 2 3    == 2
                modulus 2 (-3) == -1
-}
modulus :: Integer -> Integer -> Integer
modulus = \m n -> m `mod` n

{-
    remainder m n

    @takes      Two integer numbers
    @returns    The remainder from the quotient
    @examples   remainder 2 3    == 2
                remainder 2 (-3) == 2
-}
remainder :: Integer -> Integer -> Integer
remainder = \m n -> m `rem` n

{-
    quotient m n

    @takes      Two integer numbers
    @returns    The quotient of the two numbers
    @examples   quotient 8 4 == 2
                quotient 7 3 == 2
-}
quotient :: Integer -> Integer -> Integer
quotient = \m n -> m `quot` n