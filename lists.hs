module Lists((++),elem,last,reverse,filter) where

import Prelude hiding ((++),elem,last,reverse,filter)

{-  (++) xs ys

    Takes two lists

    RETURNS: The concatenated list
    EXAMPLES: [] ++ [1,2] == [1,2]
              [3,2] ++ [] == [3,2]
-}
(++) :: [a]->[a]->[a]
(++) = \xs ys -> foldr (\n xn -> n : xn) ys xs

{-  elem e xs

    Takes a value and a list

    RETURNS: True, if the value exists in the list otherwise returns False
    EXAMPLES: elem 5 [1,2,3,4,200,100,5] == True
              elem 6 [1,2,3,4,200,100,5] == False
-}
elem :: Eq a => a ->  [a] -> Bool
elem = \e xs -> foldl (\xs x -> x == e || xs) False xs

{-  last xs

    Takes a list

    RETURNS: Extract the last element from that list
    EXAMPLES: last [1,2,3,4,200,100,5] == 5
              last ["X"] == "X"
-}
last :: [a] -> a
last = foldl (\f x -> x) (error "empty list")

{-  reverse xs

    Takes a list

    RETURNS: List, with the same elements but in the reverse order
    EXAMPLES: reverse ["X"] == ["X"]
              reverse [1,2,3,4,200,100,5] == [5,100,200,4,3,2,1]
-}
reverse :: [a] -> [a]
reverse = \xs -> foldl (\x xs -> xs : x) [] xs

{-  filter f xs

    Takes a predicate and a list

    RETURNS: The list of elements that satisfy that predicate
    EXAMPLES: filter (>50) [1,2,3,4,200,100,5] == [200,100]
              filter (<0) [1,2,3,4,200,100,5] == []
-}
filter :: (a -> Bool) -> [a] -> [a]
filter = \f xs -> foldr (\x xs -> if f x then (x : xs) else xs) [] xs