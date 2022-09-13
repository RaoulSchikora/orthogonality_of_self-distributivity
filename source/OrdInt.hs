module OrdInt where

{- This is a modul implementing ordered pairs of chars. -}

type OrdInts = (Int, Int)
type OrdString = [OrdInts]

-- funciton to print an OrdInts
showOrdInts :: OrdInts -> String
showOrdInts (c,1) = show c
showOrdInts (c,n) = show c ++ "^" ++ show n

-- funciton to print an OrdString
showOrdStr :: OrdString -> String
showOrdStr [] = "{}"
showOrdStr xs = "{" ++ concat [(showOrdInts x) ++ "," | x <- init xs] ++ showOrdInts (last xs) ++ "}"

-- returns an OrdString where {a^n,a^m} is merged into {a^n+m} after one iteration. Helper for short
shorten :: OrdString -> OrdString
shorten [] = []
shorten ((c,0):xs) = shorten xs
shorten ((c1,n1):(c2,n2):xs) | c1 == c2 = shorten ((c1,n1+n2): shorten xs)
                             | otherwise = (c1,n1): shorten ((c2,n2):xs)
shorten (x:[]) = [x]

-- returns an OrdString where consecutive and equal elements are added by their powers. It repeats
-- the process until the length of the OrdString did not change
short :: OrdString -> OrdString
short xs | length shortend == length xs = shortend
         | otherwise = short shortend 
    where
        shortend = shorten xs

-- invert the powers of an OrdString, but does not reverse the String
invert :: OrdString -> OrdString
invert [] = []
invert ((c,n):xs) = (c,-1*n):invert xs

-- applies the power to an OrdString of the given Int
power :: OrdString -> Int -> OrdString
power xs 0 = []
power xs n | n > 0 = xs ++ power xs (n-1)
           | n < 0 = power inverted (-1*n)
    where
        inverted = invert (reverse xs)