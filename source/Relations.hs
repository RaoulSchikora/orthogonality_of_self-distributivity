module Relations where

{- This is a modul implementing relations -}

import qualified Data.Set as Set

-- adds the two relations with Mellies` definiiton of addition
add :: Ord a => [(a,a)] -> [(a,a)] -> [(a,a)]
add xs ys = Set.toList ((xsSet `Set.difference` ysSetOpp) `Set.union` ysSet) 
    where
        xsSet = Set.fromList xs
        ysSet = Set.fromList ys
        ysSetOpp = Set.fromList (opp ys)

-- returns the opposite of the given relation
opp :: [(a,a)] -> [(a,a)]
opp [] = []
opp (x:xs) = (x2,x1) : opp xs 
    where
        (x1,x2) = x

-- returns True, if the first is a subset of the second
isSubset :: Ord a => [(a,a)] -> [(a,a)] -> Bool
isSubset a b = (Set.fromList a) `Set.isSubsetOf` (Set.fromList b)

-- calculates the residual psi `after` phi (i.e. psi/phi) of the two given multisteps 
-- (list of tuples) psi and phi
after :: (Ord a) => [(a,a)] -> [(a,a)] -> [(a,a)]
after psi phi = Set.toList (Set.difference joinSet phiSet) 
    where
        joinSet = setClosure (Set.fromList (psi ++ phi)) 0 0
        phiSet = Set.fromList phi

-- calculates the join of the two given multisteps (list of tuples).
join :: (Ord a) => [(a,a)] -> [(a,a)] -> [(a,a)]
join phi psi = transClosure (phi ++ psi)

-- calculates the transitive closure of the given list of tuples.
transClosure :: (Ord a) => [(a,a)] -> [(a,a)]
transClosure xs = Set.toList (setClosure (Set.fromList xs) 0 0)

-- brute forces the transitive closure of the given set. The two counters have to be set
-- to 0 in order to start at the element at position 0,0. Brute force, since the adjacency
-- matrix is expected to be sparse, and in this case brute force has an advange complexity-
-- wise against Floyd-Warshall.
setClosure :: (Eq a, Ord a) => Set.Set((a,a)) -> Int -> Int -> Set.Set((a,a))
setClosure set i j | i >= Set.size set = set
                   | j >= Set.size set = setClosure set (i+1) 0
                   | p2 == q1 && notSubset = setClosure (Set.insert (p1,q2) set) 0 0 
                   | otherwise = setClosure set i (j+1) 
    where
        notSubset = not ((p1,q2) `Set.member` set)
        (p1,p2) = Set.elemAt i set
        (q1,q2) = Set.elemAt j set

-- Returns van Oostroms' scopic interior
scopicInterior :: (Ord a) => [(a,a)] -> [(a,a)] -> [(a,a)]
scopicInterior us ts = ts `setMinus` (transClosure (ts `setMinus` us))

-- Returns true if the given relation is transitive
isTransitive :: Ord a => [(a,a)] -> Bool
isTransitive a = setClosure aSet 0 0 == aSet 
    where
        aSet = Set.fromList a

-- calculates Mellies' bullet function of two relations
bullet :: Ord a => [(a,a)] -> [(a,a)] -> [a] -> [(a,a)]
bullet xs ys ts = Set.toList (Set.fromList bulletRel) 
    where
        bulletRel = bulletPair ts (topSet ts) xs ys

-- helper for bullet
bulletPair :: Eq a => [a] -> [(a,a)] -> [(a,a)] -> [(a,a)] -> [(a,a)]
bulletPair _ [] _ _ = []
bulletPair ts (t:top) xs ys | and (map (\y -> ((fst t,y) `elem` xs) || (y,snd t) `elem` ys) ts) = t : bulletPair ts top xs ys
                            | otherwise = bulletPair ts top xs ys

-- returns the full relation on the given set
topSet :: [a] -> [(a,a)]
topSet xs = [(x,y) | x <- xs, y <- xs ]

-- computes the complementary relation in respect to the given set
complement :: Eq a => [(a,a)] -> [a] -> [(a,a)]
complement xs n = setMinus (topSet n) xs

-- computes set difference
setMinus :: Eq a => [(a,a)] -> [(a,a)] -> [(a,a)]
setMinus [] ys = []
setMinus (x:xs) ys | x `elem` ys = setMinus xs ys
                   | otherwise   = x : setMinus xs ys

-- composes the first with the second relation
compose :: (Eq a, Ord a) => [(a,a)] -> [(a,a)] -> [(a,a)]
compose [] _ = []
compose (x:xs) ys = Set.toList (Set.fromList dotRel) 
    where
        dotRel = [(fst x, snd z)| z <- (filter (\y -> snd x == fst y) ys)] ++ compose xs ys

-- computes the full reflexive relation of the given set
delta :: [a] -> [(a,a)]
delta [] = []
delta (x:xs) = (x,x) : delta xs

-- computes the union of the two given relations
union :: Ord a => [(a,a)] -> [(a,a)] -> [(a,a)]
union xs ys = Set.toList (Set.union (Set.fromList xs) (Set.fromList ys))

-- computes the intersection of the two given relations
intersection :: Ord a => [(a,a)] -> [(a,a)] -> [(a,a)]
intersection xs ys = Set.toList (Set.intersection (Set.fromList xs) (Set.fromList ys))

-- returns true if the given relation is scopic
isScopic :: Ord a => [(a,a)] -> [a] -> Bool
isScopic a n = aSet `Set.isSubsetOf` aBullet 
    where
        aSet = Set.fromList a
        aBullet = Set.fromList (bullet a a n)

-- computes the transitive reduct of a given relation
transReduct :: Ord a => [(a,a)] -> [(a,a)]
transReduct set = Set.toList (setReduct (Set.fromList set) 0 0)

-- helper for transReduct
setReduct :: (Eq a, Ord a) => Set.Set((a,a)) -> Int -> Int -> Set.Set((a,a))
setReduct set i j | i >= Set.size set = set 
                  | j >= Set.size set = setReduct set (i+1) 0
                  | p2 == q1 && isSubset = setReduct (Set.delete (p1,q2) set) 0 0
                  | otherwise = setReduct set i (j+1)
    where
        (p1,p2) = Set.elemAt i set
        (q1,q2) = Set.elemAt j set
        isSubset = (p1,q2) `Set.member` set