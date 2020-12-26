module SyntaxInFunctions(iota, inter, interOrdered, Fruit(Apple,Banana,Lemon), sumPrice) where

{-  iota n
    Get a list of numbers from 1 to n-1

    RETURNS: list
    EXAMPLES: iota 5 == [0, 1, 2, 3, 4]
-}
iota :: (Eq t, Num t) => t -> [t]
iota 0 = []
iota n = iota (n-1) ++ [n-1]

{- inter s1 s2
    Takes two arrays and get a new list of only the elements that are in s1 and s2

    RETURNS:  s1 ∩ s2
    EXAMPLES: inter [2,1] [1] == [1]
-}
inter :: Eq a => [a] -> [a] -> [a]
inter [] _ = []
inter (x:xs) ys
    | x `elem` ys = x : inter xs ys
    | otherwise = inter xs ys

{-  interOrdered s1 s2

    Takes two ordered list and get a new list of only the elements that are in s1 and s2

    RETURNS:  s1 ∩ s2
    EXAMPLES: interOrdered [2,1] [1] == [1]
-}
interOrdered :: Ord a => [a] -> [a] -> [a]
interOrdered [] _ = []
interOrdered _ [] = []
interOrdered (x:xs) (y:ys)
        | x == y    = x : interOrdered xs ys
        | x > y     = interOrdered (x:xs) ys
        | otherwise = interOrdered xs (y:ys)

{-  sumPrice xf a b l

    Takes a list of fruit, the price of apples, bananas, and the price of lemons

    RETURNS:  The total cost of the items in the list
    EXAMPLES: sumPrice [] 3.0 2.0 5.0 == 0.0
              sumPrice [Banana 4.0, Apple 3.0, Lemon 7, Banana 2.0, Apple 1.0, Lemon 1] 3.0 2.0 5.0 == 64.0
    VARIANTS: xf
-}
data Fruit = Apple Double | Banana Double | Lemon Integer

sumPrice :: [Fruit] -> Double -> Double -> Double -> Double

sumPrice ([]) _ _ _ = 0.0
sumPrice ([Apple unit]) price _ _ = price * unit
sumPrice ([Banana unit]) _ price _ = price * unit
sumPrice ([Lemon unit]) _ _ price = price * fromInteger(unit)
sumPrice (f:xf) a b l = sumPrice [f] a b l + sumPrice xf a b l