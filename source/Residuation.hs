module Residuation where

import qualified Data.Set as Set

-- calculates the residual psi `after` phi (i.e. psi/phi) of the two given multisteps 
-- (list of tuples) psi and phi
after :: (Ord a) => [(a,a)] -> [(a,a)] -> [(a,a)]
after psi phi = Set.toList (Set.difference joinSet phiSet) where
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
                   | otherwise = setClosure set i (j+1) where
                       notSubset = not (Set.isSubsetOf (Set.fromList [(p1,q2)]) set)
                       (p1,p2) = Set.elemAt i set
                       (q1,q2) = Set.elemAt j set
