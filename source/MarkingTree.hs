module MarkingTree where

{- This is a module implementing marking trees. A marking tree is a representation of a trek (t,U), 
where first and second components of the position pairs in U are marked in t, i.e., the trek
        (1.2.3.4, {(\epsilon,0),(\epsilon,00),(0,00)})
has the marking tree where position \epsilon is doubly marked as first poisiton, 0 is marked as 
first and second position, and 00 is doubly marked as second position. -}

import Multiset
import OrdInt
import Rd
import Data.List
import Relations

-- data to mark first and second components of U in a term t for a trek (t,U)
data Mtree =
    Mv Int
    | Mt Mtree Mtree (Int,Int)
    deriving Eq

instance Show Mtree where
    show (Mv v) = show v
    show (Mt t1 t2 p) = drawFuncSym p ++ "(" ++ show t1 ++ "," ++ show t2 ++ ")"

-- helper to draw a funciton
drawFuncSym :: (Int,Int) -> String
drawFuncSym n = "*" ++ show (fst n) ++ "|" ++ show (snd n)

-- creates a marking tree for a given term
termToMtree :: Term -> Mtree
termToMtree (V v) = (Mv 1)
termToMtree (T t0 t1) = Mt (termToMtree t0) (termToMtree t1) (1,1)

-- mark second position
markSndPos :: Pos -> Mtree -> Int -> Mtree
markSndPos _ (Mv v) _ = error "input position to markPos too long"
markSndPos [] (Mt t0 t1 p) n = Mt t0 (addLeaf t1 n) (fst p, snd p + 1)
markSndPos (0:pos) (Mt t0 t1 p) n = Mt (markSndPos pos t0 n) t1 p
markSndPos (1:pos) (Mt t0 t1 p) n = Mt t0 (markSndPos pos t1 n) p

-- creates an Mtree out of a trek. Before marking it orders the pairs according to revOrd
trekToMtree :: Trek -> Mtree
trekToMtree (t,u) = mark (sortBy (revOrdPair) u) (termToMtree t)

-- marks the position pairs in given order
mark :: [(Pos,Pos)] -> Mtree -> Mtree
mark [] mt = mt
mark (u:us) mt = mark us (markPos u mt)

-- marks a given position pair
markPos :: (Pos,Pos) -> Mtree -> Mtree
markPos (_,_) (Mv v) = error "not a valid position in the tree"
markPos ([],0:pos) (Mt t0 t1 p) = Mt (markSndPos pos t0 (getLeafSum t1)) t1 (fst p + 1,snd p)
markPos ((0:as),(0:bs)) (Mt t0 t1 p) = Mt (markPos (as,bs) t0) t1 p
markPos ((1:as),(1:bs)) (Mt t0 t1 p) = Mt t0 (markPos (as,bs) t1) p

-- comparing positions according to pre-Order tree traversal. First visited nodes are smaller than
-- nodes visited later
postOrd :: Pos -> Pos -> Ordering
postOrd [] [] = EQ
postOrd [] (0:p) = GT
postOrd (0:p) [] = LT
postOrd [] (1:p) = GT
postOrd (1:p) [] = LT
postOrd (0:p) (1:q) = LT
postOrd (1:p) (0:q) = GT
postOrd (_:p) (_:q) = postOrd p q

-- comparing positions according to reversed in-order tree traversal.
revOrd :: Pos -> Pos -> Ordering
revOrd [] [] = EQ
revOrd [] (0:p) = LT
revOrd (0:p) [] = GT
revOrd [] (1:p) = GT
revOrd (1:p) [] = LT
revOrd (0:p) (1:q) = GT
revOrd (1:p) (0:q) = LT
revOrd (_:p) (_:q) = revOrd p q

-- comparing position pairs according to post-order of their first components
postOrdPair :: (Pos,Pos) -> (Pos,Pos) -> Ordering
postOrdPair p q = postOrd (fst p) (fst q)

-- comparing position pairs according reversed in-order tree traversal on second component followed by
-- comparision on first component
revOrdPair :: (Pos,Pos) -> (Pos,Pos) -> Ordering
revOrdPair p q | (snd p) == (snd q) = revOrd (fst p) (fst q)
               | otherwise = revOrd (snd p) (snd q)

-- returns the sum of the variables of the Mtree
getLeafSum :: Mtree -> Int
getLeafSum (Mv v) = v
getLeafSum (Mt t0 t1 _) = getLeafSum t0 + getLeafSum t1

-- adds the given Int to the leftmost variabel
addLeaf :: Mtree -> Int -> Mtree
addLeaf (Mv v) n = Mv (v + n)
addLeaf (Mt t0 t1 p) n = Mt (addLeaf t0 n) t1 p

-- sets the weights of the leafs -> done after having marked all position pairs
setLeafs :: Mtree -> Int -> (Mtree,Int)
setLeafs (Mv v) n = (Mv (v + n),v + n)
setLeafs (Mt t0 t1 p) n = (Mt t0Set t1Set p,getLeftMostVar t0Set)
    where
        (t1Set,value1) = setLeafs t1 n
        (t0Set,value0) = setLeafs t0 value1

-- returns the Int of the leftmost variabel
getLeftMostVar :: Mtree -> Int
getLeftMostVar (Mv v) = v
getLeftMostVar (Mt t0 t1 p) = getLeftMostVar t0

-- cost function to calculate the cost of a marking tree
cost :: Mtree -> OrdString
cost (Mv v) = [(v,1)]
cost (Mt t0 t1 p) = short (ord3 ++ ord2 ++ ord1)
    where
        ord1 = cost t1
        ord2 = power (cost t0) (snd p)
        ord3 = invert ord1

-- returns the multiset representation of an ordered String
ordToMs :: OrdString -> Ms
ordToMs [] = []
ordToMs (x:xs) | snd x >= 0 = incrementBy (fst x) (snd x) (ordToMs xs)
               | otherwise  = ordToMs xs

-- creates the multiset representation of a given trek and a given mapping Char->Int
trekToMs :: Trek -> Ms
trekToMs (t,u) = ordToMs (cost tree)
    where 
        (tree,value) = setLeafs (trekToMtree (t,u)) 0

----------------------------------------------------------------------------------------------------
--                   Norm
----------------------------------------------------------------------------------------------------

-- returns the norm of a trek, which is a multiset
norm :: Trek -> Ms
norm (t,u) = trekToMs (t,u)
