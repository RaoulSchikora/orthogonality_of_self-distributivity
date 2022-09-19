module Reader where

{- This is a module to read and handle input by the user. Especially, it provides a 
way to convert a string into a Term. A term as a string underlies the following grammar:
        t -> (t.t) | v 
where v is any string without '.','(' or ')'. Outer parentheses can be dropped. Terms 
associate to the left, i.e. "1.2.3.4" is equivalent to "((1.2).3).4". -}

import Rd
import Data.Char

{- ------------ ------------ Reading  ------------ ------------ -}

        {- ------------ string to term ------------ -}

-- converts a string into tokens neccessary for parsing a term. Since Terms are left associative
-- we need to read the term from right to left. For reason of speed we reverse it once at the 
-- beginning, which is why the Char ')' becomes the String "(". Similar for '('.
strToTokens :: String -> [String]
strToTokens [] = []
strToTokens (x:xs) | x == ' ' = strToTokens xs
                     | x == '(' = [")"] ++ (strToTokens xs)
                     | x == '.' = ["."] ++ (strToTokens xs)
                     | x == ')' = ["("] ++ (strToTokens xs)
                     | otherwise = [(getNextVar (x:xs))] ++ 
                        (strToTokens (dropWhile (\x-> not (isParDot [x])) xs))

-- returns True if the given String consists of a single dot, i.e., "."
isDot :: String -> Bool
isDot x | x == "."  = True
         | otherwise = False

-- returns True if the given String consists of an opening parentheses, i.e., "("
isOPar :: String -> Bool 
isOPar x | x == "("  = True
          | otherwise = False

-- returns True if the given String consists of a closing parentheses, i.e., ")"
isCPar :: String -> Bool
isCPar x | x == ")"  = True
          | otherwise = False

-- retruns True if the given String consists of a parentheses
isPar :: String -> Bool
isPar x | isOPar x || isCPar x = True
         | otherwise                = False

-- returns True if the given String is a blank character, i.e., " "
isBlank :: String -> Bool
isBlank x | x == " "  = True
           | otherwise = False

-- returns True if the given String is a parentheses or a dot, i.e., "." or "(" or ")"
isParDot :: String -> Bool
isParDot x | isPar x || isDot x = True
             | otherwise            = False

-- returns True is the given String is a variable
isVar :: String -> Bool
isVar x | not (isParDot x) = True
         | otherwise          = False

-- returns the next variable, if it is no variable an empty string is returned.
getNextVar :: String -> String
getNextVar []     = []
getNextVar (x:xs) = if not(isParDot [x]) then x : getNextVar xs else []

-- Helper for tokensToTerm. Because we associate terms to the left we reverse them during
-- parsing, hence we call T (rhs) (lhs), i.e. the sides are off.
getTerm :: [String] -> [String] -> Int -> Term
getTerm _ _ (-1) = error ("Missing parentheses: (")
getTerm lhs [] i | i == 0 =  getTerm [] ((tail . init) lhs) 0
                  | otherwise = error ("Missing parentheses: )")
getTerm lhs (r:rhs) i | i == 0 && isDot r = T (tokensToTerm rhs) (tokensToTerm lhs)
                       | otherwise          = getTerm (lhs ++ [r]) rhs counter where
                           counter | isOPar r = i+1
                                   | isCPar r = i-1
                                   | otherwise = i

-- converts a stream of tokens to terms. Parses associating to the left. If the stream
-- is not properly formed an error is thrown. Wrapper for getTerm.
tokensToTerm :: [String] -> Term
tokensToTerm (x:[]) | isVar x  = V x
                      | otherwise = error ("expected variable but encountered " ++ x)
tokensToTerm xs = getTerm [] xs 0

-- returns a term to a given string. The revesed stream of tokens is passed on, because
-- of reason of speed.
parse :: String -> Term
parse str = (tokensToTerm . reverse . strToTokens) str

        {- ------------ string to steps ------------ -}

-- reads a digit and throws an error if it is not 0 or 1
readDigit :: String -> Int
readDigit str | digit == 0 || digit == 1 = digit
              | otherwise = error ("Pos is required to consist of \'0\' ot \'1\' only but encountered " ++ str) where
                  digit = read str :: Int

-- Splits a string of digits into a list of digits. E.g. "111" -> [1,1,1].
-- An error is thrown if the string contains non-digit characters.
strToPos :: String -> Pos
strToPos [] = []
strToPos (x:xs) | isDigit x = readDigit [x] : strToPos xs
                | otherwise = error ("Pos not in required form. Expected a digit but encountered " ++ [x])

-- transform a simicolon seperated string into a list of tuples, e.g.
-- "00,,01" -> [[0,0],[],(0,1)]
parseRules :: String -> [Pos]
parseRules str = map (strToPos) splittedStr where
    splittedStr = split ',' (filter (/=' ') str)

-- Split a string at the given Char. Taken from
-- https://www.reddit.com/r/haskell/comments/809a1r/splitting_problem/
split :: Char -> String -> [String]
split c = map (takeWhile (/= c)) . takeWhile (not . null) . 
    iterate (dropWhile (==  ' ') . drop 1 . dropWhile (/= c))
