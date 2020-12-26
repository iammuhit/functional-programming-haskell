{-
    displayMe x
    
    @takes      An input of any kind
    @returns    The value of x
    @examples   displayMe 3 == 3
                displayMe "Haskell for Great Good!" == "Haskell for Great Good!"
-}
displayMe x = x

{-
    doubleMe x
    
    @takes      A number
    @returns    Multiplies the number by two
    @examples   doubleMe 5 == 10
                doubleMe 2.5 == 5.0
-}
doubleMe x = x + x

{-
    doubleUs x y
    
    @takes      Two numbers
    @returns    Multiplies the each number by two and then adds them together
    @examples   doubleUs 4 5 == 18
                doubleUs 2.5 3.0 == 12.0
-}
doubleUs x y = doubleMe x + doubleMe y
