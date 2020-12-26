module StringMatch(isMatch) where

{-  isMatch s1 s2

    Takes two strings or list of characters

    RETURNS: True, if s1 matches according to the patterns of s2, otherwise returns False
    VARIANTS: length xs, length xp
    EXAMPLES: isMatch "aa" "a" == False
              isMatch "acdcb" "a*c?b"
-}
isMatch :: String -> String -> Bool
isMatch [] [] = True
isMatch [] _ = False
isMatch _ [] = False
isMatch (s:xs) (p:xp) 
    | p == '\0' && s == '\0'                  = True
    | p == '?' || s == p                      = isMatch xs xp
    | p == '*' && length xp /= 0 && s == '\0' = False
    | p == '*' && length xp == 0              = True
    | p == '*'                                = isMatch (s:xs) xp || isMatch xs (p:xp)
    | otherwise                               = False