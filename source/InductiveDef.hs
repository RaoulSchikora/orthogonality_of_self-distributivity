module InductiveDef where

{- This is a modul implementing an idea to inductively compute the target of a trek's full development.
    It only comprises ideas of the author and has not been proven to be correct. -}

import Rd
import Relations
import Data.List

-- inductively computes a target of a complete development of a trek
complete :: Trek -> Trek
complete (t,u) = (unfold t u, [])

-- helper for the funciton 'complete' inductively unfolding the multi-step of a term. Espeacially, it
-- passes the transitve reduct to the function 'unfolding'.
unfold :: Term -> [(Pos,Pos)] -> Term
unfold t u = unfolding t t [] tr
    where
        tr = transReduct u

-- returns the sub-term of the given term at the given position
getSubTerm :: Term -> Pos -> Term
getSubTerm t [] = t
getSubTerm (T t0 t1) (0:xs) = getSubTerm t0 xs
getSubTerm (T t0 t1) (1:xs) = getSubTerm t1 xs

-- returns true if the given position is an end of a path in the given set of positions.
isPathEnd :: Pos -> [(Pos,Pos)] -> Bool
isPathEnd p u = (filter ((==p).snd) u /= []) && (filter ((==p).fst) u == [])

-- returns true if the given position is a start of a path in the given set of positons.
isPathStart :: Pos -> [(Pos,Pos)] -> Bool
isPathStart p u = filter ((==p).snd) u == [] && (filter ((==p).fst) u /= [])

-- retuns true if the given position is neither a start nor an end of paths in the given
-- set of positions.
isOnPath :: Pos -> [(Pos,Pos)] -> Bool
isOnPath p u = (filter ((==p).snd) u /= []) && (filter ((==p).fst) u /= [])

-- distributes the term 's' according to the given 'char' over the given term.
--      b: distributes over both
--      l: distributes only on the left
--      r: distributes only on the right
--      n: distributes over none.
distr :: Char -> Term -> Term -> Term
distr _ (V v) s = T (V v) s
distr 'b' (T t0 t1) s = T (T t0 s) (T t1 s)
distr 'l' (T t0 t1) s = T (T t0 s) t1
distr 'r' (T t0 t1) s = T t0 (T t1 s)
distr 'n' (T t0 t1) s = T t0 t1

-- distributes the term 's' according to the power of the given string. For the meaning of
-- b,l,r,n see the function 'distr'.
distrPower :: String -> Term -> Term -> Term
distrPower _ (V v) s = T (V v) s
distrPower [x] t s = distr x t s
distrPower ('b':xs) (T t0 t1) s = T (distrPower xs t0 s) (distrPower xs t1 s)
distrPower ('l':xs) (T t0 t1) s = T (distrPower xs t0 s) t1
distrPower ('r':xs) (T t0 t1) s = T t0 (distrPower xs t1 s)
distrPower ('n':xs) (T t0 t1) s = T t0 t1

-- helper for unfold. The initial term 'initialT' is not being modified. The funcition recursively calls itself
-- depending on the position. If the position is an end of a path in the transitive reduct it follows the path up,
-- if it is a position on a path it recursively calls the funciton again on the left sub term. If the position is 
-- on no path the function calls itself recursively in both sub-terms (left and right).
unfolding :: Term -> Term -> Pos -> [(Pos,Pos)] -> Term
unfolding _ (V v) _ _ = V v
unfolding initialT (T t0 t1) pos u | isPathEnd pos u = followPathUp [] initialT (T unfoldT0 unfoldT1) pos u
                                   | isPathStart pos u || isOnPath pos u = unfoldT0
                                   | otherwise = T unfoldT0 unfoldT1
    where
        unfoldT0 = unfolding initialT t0 (pos++[0]) u
        unfoldT1 = unfolding initialT t1 (pos++[1]) u

followPathUp :: String -> Term -> Term -> Pos -> [(Pos,Pos)] -> Term
followPathUp str initialT newT p u | upperLinks == [] = newT
                                   | otherwise = followPathUp str initialT (T s0 s1) p (delete (head upperLinks) u)
    where
        upperLinks = sortOn (((length p)-).length.fst) (filter ((==p).snd) u)
        fstUpperLink = (fst.head) upperLinks
        leftOfFstUpperLinkEx = or (map (isPrfx (((snd.head) upperLinks)++[0])) (map (snd) (filter ((==fstUpperLink).fst) u)))
        rightOfFstUpperLinkEx = or (map (isPrfx (((snd.head) upperLinks)++[1])) (map (snd) (filter ((==fstUpperLink).fst) u)))
        leftAndRightEx = leftOfFstUpperLinkEx && rightOfFstUpperLinkEx
        ext = if leftAndRightEx then 'n' else (if leftOfFstUpperLinkEx then 'r' else (if rightOfFstUpperLinkEx then 'l' else 'b'))
        unfoldedSubTerm = unfolding initialT (getSubTerm initialT (fstUpperLink++[1])) (fstUpperLink++[1]) u
        distrbiuteOverNew = distrPower (str++[ext]) newT unfoldedSubTerm
        T s0 s1 = followPathUp (str++[ext]) initialT distrbiuteOverNew fstUpperLink u
