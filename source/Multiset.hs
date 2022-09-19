module Multiset where

{- This is a module implementing multisets, interpreted as function over the natrual numbers.
The show funciton displays the multiset in the more readable form {0,1,1,1,1,2,2,3}, which
represents the multiset M with M(0)=1, M(1)=4, M(2)=2, M(3)=1. -}

type Ms = [Int]

-- show function for multisets
showMs :: Ms -> String
showMs [] = "\8709"
showMs xs = "{" ++ showMsWrap xs 0 False

-- helper for show function, bool indicates if a comma needs to be set at the beginning 
-- of the string
showMsWrap :: Ms -> Int -> Bool -> String
showMsWrap [] _ _ = "}"
showMsWrap (x:xs) n com = (entry n x com) ++ showMsWrap xs (n+1) (com || not (x == 0))

-- helper for wrapper, bool indicates if a comma needs to be set at the beginning of the str
entry :: Int -> Int -> Bool -> String
entry num 0 _ = ""
entry num 1 com | com = "," ++ show num
                | otherwise = show num
entry num times com | com = "," ++ show num ++ showPower times
                    | otherwise = show num ++ showPower times

-- converts the power into a string
showPower :: Int -> String
showPower n = "^" ++ show n

-- adds two multisets of arbitrary length
addMs :: Ms -> Ms -> Ms
addMs ms1 [] = ms1
addMs [] ms2 = ms2
addMs (m1:ms1) (m2:ms2) = (m1+m2) : addMs ms1 ms2

-- monus on ints
monus :: Int -> Int -> Int
monus n m | n < m = 0
          | otherwise = n - m

-- calculates the complement of the first from the second, i.e., Ms1 - Ms2
complMs :: Ms -> Ms -> Ms
complMs ms1 [] = ms1
complMs [] ms2 = []
complMs (m1:ms1) (m2:ms2) = (m1 `monus` m2) : complMs ms1 ms2

-- is the first Multiset greater than the second according to the multiset extension,
-- which is equivalent to saying that the reversed list of the first multiset is 
-- lexicographic smaller to the reversed list of the second multiset
greater :: Ms -> Ms -> Bool
greater m1 m2 = lexicoGreater (reverse (m1 ++ zeros1)) (reverse (m2 ++ zeros2)) where
    len1 = length m1
    len2 = length m2
    len = max len1 len2
    zeros1 = zeros (len - len1)
    zeros2 = zeros (len - len2)

-- checks if the first is lexicographic greater than the second, given the elmenets of
-- the lists are comparable
lexicoGreater :: (Ord a) => [a] -> [a] -> Bool
lexicoGreater [] [] = False
lexicoGreater [] (y:ys) = True
lexicoGreater (x:xs) [] = False
lexicoGreater (x:xs) (y:ys) | x > y = True
                            | x == y = lexicoGreater xs ys
                            | x < y = False

-- helper function returning a list of zero with length of the given Int
zeros :: Int -> [Int]
zeros 0 = []
zeros n = 0 : zeros (n-1)

-- increments the multisets counter at the position of the given Int by 1
increment :: Int -> Ms -> Ms
increment 0 (m:ms) = (m+1):ms
increment 0 [] = [1]
increment n [] = 0 : increment (n-1) []
increment n (m:ms) = m : increment (n-1) ms

-- increments the multisets counter at the position of the first int by the second int
incrementBy :: Int -> Int -> Ms -> Ms
incrementBy 0 n (m:ms) = (m+n):ms
incrementBy 0 n [] = [n]
incrementBy counter n [] = 0 : incrementBy (counter-1) n []
incrementBy counter n (m:ms) = m : incrementBy (counter-1) n ms
