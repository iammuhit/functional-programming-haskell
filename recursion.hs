module Recursion(product) where

import Prelude hiding (product)

{-  product n
    
    TAKES:    A number
    RETURNS:  Computes a value of the series: 1 * 2 * 3 * ... * n
    PRE:      n >= 1
    VARIANT:  n
    EXAMPLES: product 3 == 6
              product 5 == 120
-}
product n = 
    if n == 1 then
        1
    else n * product (n - 1)