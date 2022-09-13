module Rd where

import Relations
import qualified Data.Set as Set
import System.Random

{- This is a module with basic RD operations. RD refers to right self-distributivity, i.e. a rewrite system
with the single rule
        (x.y).z -> (x.z).(y.z)
Furthermore RD-multisteps are implemented. -}

type Pos = [Int]

showPos :: Pos -> String
showPos [] = "\949"
showPos xs = concat (map (show) xs)

showPosSeq :: [Pos] -> String
showPosSeq [] = []
showPosSeq [x] = showPos x
showPosSeq (x:xs) = (showPos x) ++ "," ++ showPosSeq xs

showPair :: [(Pos,Pos)] -> String
showPair [] = " \8709 "
showPair xs = "{" ++ (showPairWrap xs) ++ "}"

showPairWrap :: [(Pos,Pos)] -> String
showPairWrap [] = ""
showPairWrap [x] =  "(" ++ (showPos (fst x)) ++ "," ++ (showPos (snd x)) ++ ")"
showPairWrap (x:xs) = "(" ++ (showPos (fst x)) ++ "," ++ (showPos (snd x)) ++ ")," ++ showPairWrap xs

data Term = 
    V String
    | T Term Term
    deriving Eq

instance Show Term where
    show (V v)     = id v
    --show (T t0 t1) = "\8226" ++ show t0 ++ show t1
    show (T t0 t1) = showLeft t0 ++ "\9667" ++ showRight t1

-- show function to omit parentheses on the left (terms associate to the left)
showLeft :: Term -> String
showLeft (V v) = id v
showLeft (T t0 t1) = showLeft t0 ++ "\9667" ++ showRight t1

-- show function to parenthesize on the right of terms
showRight :: Term -> String
showRight (V v) = id v
showRight (T t0 t1) = "(" ++ showLeft t0 ++ "\9667" ++ showRight t1 ++ ")"

type Trek = (Term,[(Pos,Pos)])

showTrek :: Trek -> String
showTrek (t,u) = "(" ++ show t ++ "," ++ showPair u ++ ")"

-- returns the length of a term, i.e. counts the variables. 
len :: Term -> Int
len (V v) = 1
len (T t0 t1) = len t0 + len t1

-- returns the height of a term.
height :: Term -> Int
height (V v) = 0
height (T t0 t1) = 1 + max (height t0) (height t1)

-- applies rd to the given term
rd :: Term -> Term
rd (V v) = error ("Rd not applicable to variable " ++ show (V v))
rd (T (T t0 t1) t2) = T (T (t0) (t2)) (T (t1) (t2))
rd (T t0 t1) = error ("Rd not applicable to given term: " ++ show (T t0 t1))

-- applies rd at given position to given term
rdat :: Term -> Pos -> Term
rdat t [] = rd t
rdat (V v) xs = error ("Non-valid position " ++ show xs ++ ".")
rdat (T t0 t1) (x:xs) | x < 0 || x > 1 = error ("Positional element: " ++ show x ++ ". Positional elements have to be 0 or 1.")
                      | x == 0 = T (rdat t0 xs) t1
                      | x == 1 = T t0 (rdat t1 xs)

-- apply a sequence of positions successively to a term
rdSeq :: Term -> [Pos] -> Term
rdSeq t [] = t
rdSeq t (x:xs) = rdSeq (rdat t x) xs

-- returns a list of all positions of the term
getSkeleton :: Term -> [Pos]
getSkeleton t = getSkelWrap t []

-- helper for getSkeleton
getSkelWrap :: Term -> Pos -> [Pos]
getSkelWrap (V v) pos = [] -- [pos]
getSkelWrap (T t0 t1) pos = pos : (skeleton0 ++ skeleton1) 
    where
        skeleton0 = getSkelWrap t0 (pos ++ [0])
        skeleton1 = getSkelWrap t1 (pos ++ [1])

-- dilatation operator of Dehornoy
dilatation :: Term -> Term
dilatation (V v) = (V v)
dilatation (T t0 t1) = distribution (dilatation t0) (dilatation t1)

-- uniformly distributes second term in the first
distribution :: Term -> Term -> Term
distribution (V v) t = T (V v) t
distribution (T t0 t1) s = T (distribution t0 s) (distribution t1 s)

-- returns the number of non-variable nodes in a term
size :: Term -> Int
size (V v) = 0
size (T t0 t1) = 1 + size t0 + size t1

-- checks if the first position is a strict prefix of the second
isStrctPrfx :: Pos -> Pos -> Bool
isStrctPrfx [] [] = False
isStrctPrfx (x:xs) [] = False
isStrctPrfx [] (y:ys) = True
isStrctPrfx (x:xs) (y:ys) | x == y = xs `isStrctPrfx` ys
                          | x /= y = False

-- checks if the first position is a non-strict prefix of the second
isPrfx :: Pos -> Pos -> Bool
isPrfx [] [] = True
isPrfx (x:xs) [] = False
isPrfx [] (y:ys) = True
isPrfx (x:xs) (y:ys) | x == y = xs `isPrfx` ys
                     | x /= y = False

-- turns a list of positions into a relation of positions partially ordered by the prefix relation
prefRel :: [Pos] -> [(Pos,Pos)]
prefRel xs = prefRelWrap [ (x,y) | x <- xs, y <- xs ]

-- helper for prefRel
prefRelWrap :: [(Pos,Pos)] -> [(Pos,Pos)]
prefRelWrap [] = []
prefRelWrap ((p,q):xs) | p `isStrctPrfx` q = (p,q) : prefRelWrap xs
                       | otherwise         = prefRelWrap xs

-- turns a term into a partial order of positions
getParOrd :: Term -> [(Pos,Pos)]
getParOrd t = prefRel (getSkeleton t)

-- for a pair (a,b) all pairs of the form (a',b) where a' is a prefix of a, are created.
addPrefixPairs :: (Pos,Pos) -> [(Pos,Pos)]
addPrefixPairs ([],q:qs) = error ("Error: " ++ (showPos (q:qs)) ++ " is not a prefix of " ++ (showPos []))
addPrefixPairs (p,q) | p == q = []
                     | q `isStrctPrfx` p = (p,q) : addPrefixPairs (init p,q)
                     | otherwise = error ("Error: " ++ (showPos q) ++ " is not a prefix of " ++ (showPos p))

getFullTrek :: Term -> Trek
getFullTrek t = (t,filter (sd4) (getParOrd t))

----------------------------------------------------------------------------------------------------
--                   Definition check
----------------------------------------------------------------------------------------------------

-- cuts the prefix alpha off of the given Position. If alpha is not a Prefix
-- behaviour undefined, i.e. user has to check that alpha really is a Prefix.
cutOff :: Pos -> Pos -> Pos
cutOff [] pos = pos
cutOff (a:alpha) (p:pos) | a == p    = cutOff alpha pos
                         | otherwise = error ("alpha not a prefix of given position.")

-- returns True if the second componenent is in the right component of the first 
isRightOf :: Pos -> Pos -> Bool
isRightOf p q | q `isStrctPrfx` p = head (q `cutOff` p) == 1
              | otherwise = False

-- returns true if the first component is in the left subtree of the second
sd4 :: (Pos,Pos) -> Bool
sd4 (p,q) | p `isStrctPrfx` q && delta == 0 = True
          | otherwise                       = False
    where
        delta:_ = p `cutOff` q

-- checks that no element on the spine is "swapped" with the parent of the spine
spineParent :: [(Pos,Pos)] -> Bool
spineParent [] = True
spineParent ((p1,p2):pos) = sd4 (p1,p2)

-- checks for a given pair (Term,PosPairs) if SD1 to SD4 is fulfilled. Returns a quadruple of
-- booleans, where the boolean at position i refers to SDi
defCheck :: Trek -> (Bool,Bool,Bool,Bool)
defCheck (t,u) = (u `isSubset` tt, isTransitive u, isTransitive tMinusU, spineParent u) 
    where
        tt = getParOrd t
        tMinusU = tt `setMinus` u

----------------------------------------------------------------------------------------------------
--                   Tracing
----------------------------------------------------------------------------------------------------

-- traces a trek under a given position p
trace :: Trek -> Pos -> Trek
trace (t,u) p = ((rdat t p), traceU u p)

-- traces a given set of position pairs under a given step p
traceU :: [(Pos,Pos)] -> Pos -> [(Pos,Pos)]
traceU us p = delNonPrefix [(u1,u2)| u <- us, u1 <- tracePos (fst u) p, u2 <- tracePos (snd u) p]

-- traces the position under rule alpha
tracePos :: Pos -> Pos -> [Pos]
tracePos [] (a:alpha) = [[]]
tracePos [] []        = [[0],[1]]
tracePos [0] []       = [[]]
tracePos (1:pos) []   = [0:1:pos,1:1:pos]
tracePos (0:1:pos) [] = [1:0:pos]
tracePos pos []       = [pos]
tracePos (p:pos) (a:alpha) | a == p    = map (p:) (tracePos pos alpha)
                           | otherwise = [p:pos]

-- deletes all pairs, where the first component is no strict prefix of the second
delNonPrefix :: [(Pos,Pos)] -> [(Pos,Pos)]
delNonPrefix [] = []
delNonPrefix (u:us) | (fst u) `isStrctPrfx` (snd u) = u : delNonPrefix us
                    | otherwise                     = delNonPrefix us

----------------------------------------------------------------------------------------------------
--                   Strategies
----------------------------------------------------------------------------------------------------

-- the default strategy
defaultStr :: [(Pos,Pos)] -> Pos
defaultStr []     = error "No redex found"
defaultStr (u:us) | q == [0]  = (fst u)
                  | otherwise = defaultStr us 
    where
        q = (fst u) `cutOff` (snd u)

-- collects all redexes of a given set of position pairs
collectRedexes :: [(Pos,Pos)] -> [Pos]
collectRedexes []     = []
collectRedexes (u:us) | q == [0]  = (fst u) : collectRedexes us
                      | otherwise = collectRedexes us
    where 
        q = (fst u) `cutOff` (snd u)

-- In-order strategy
inOrderStr :: [(Pos,Pos)] -> Pos
inOrderStr [] = error "No redex found"
inOrderStr us = smallestInO (collectRedexes us)

-- innermost strategy
inMoStr :: [(Pos,Pos)] -> Pos
inMoStr [] = error "No redex found"
inMoStr us = innerMost (collectRedexes us)

-- outermost Strategy
outMoStr :: [(Pos,Pos)] -> Pos
outMoStr [] = error "No redex found"
outMoStr us = outerMost (collectRedexes us)

-- reversed in-order strategy
revOrderStr :: [(Pos,Pos)] -> Pos
revOrderStr [] = error "No redex found"
revOrderStr us = biggestInO (collectRedexes us)

        ----------- orders on positions ---------------

-- returns the smalles position according to in-order tree traversal
smallestInO :: [Pos] -> Pos
smallestInO []        = error "Something went wrong, the list is not supposed to be empty"
smallestInO (p:[])    = p
smallestInO (p:q:pos) | p `smallerInO` q = smallestInO (p:pos)
                      | otherwise        = smallestInO (q:pos)

-- returns the biggest position according to in-order tree traversal
biggestInO :: [Pos] -> Pos
biggestInO [] = error "List not supposed to be empty"
biggestInO (p:[]) = p
biggestInO (p:q:pos) | p `smallerInO` q = biggestInO (q:pos)
                     | otherwise        = biggestInO (p:pos)

-- True, if the first position is smaller than the second position according to in-order tree 
-- traversal
smallerInO :: Pos -> Pos -> Bool
smallerInO [] [] = False
smallerInO (p:pos) [] | p == 0 = True
                      | otherwise = False
smallerInO [] (q:qos) | q == 1 = True
                      | otherwise = False
smallerInO (p:pos) (q:qos) | p == q    = pos `smallerInO` qos
                           | p == 0    = True
                           | otherwise = False

-- returns an innermost postion
innerMost :: [Pos] -> Pos
innerMost []        = error "Something went wrong, the list should not be empty"
innerMost (p:[])    = p
innerMost (p:q:pos) | p `isPrfx` q = innerMost (q:pos)
                    | otherwise    = innerMost (p:pos)

-- returns an outermost position
outerMost :: [Pos] -> Pos
outerMost []        = error "Someting went wrong, shouldn't be empty"
outerMost (p:[])    = p
outerMost (p:q:pos) | p `isPrfx` q = outerMost (p:pos)
                    | otherwise    = outerMost (q:pos)

----------------------------------------------------------------------------------------------------
--                   Developments
----------------------------------------------------------------------------------------------------

-- returns the chosen redex to the given strategy, indicated by an Int
-- 0 -> Default Strategy
step :: Trek -> Int -> Pos
step (t,u) 0 = defaultStr u
step (t,u) 1 = inOrderStr u
step (t,u) 2 = inMoStr u
step (t,u) 3 = revOrderStr u
step (t,u) 4 = outMoStr u
step (t,u) n = error ("Strategy with number " ++ show n ++ " not implemented.")

-- develops a Trek according to given strategy indicated by an Int (see step) and returns a list 
-- with all intermediate Treks and the resulting step from that trek
develop :: Trek -> Int -> [(Trek,Pos)]
develop (t,[]) _ = [((t,[]),[])]
develop (t,u) n  = ((t,u),pos) : develop (trace (t,u) pos) n
    where
        pos = step (t,u) n
