module Testsuit where

import Reader
import Rd
import Multiset
import MarkingTree
import InductiveDef
import Residuation

----------------------------------------------------------------------------------------------------
--                   Test
----------------------------------------------------------------------------------------------------

-- draws a tick for True and a cross for False
tickOrCross :: Bool -> String
tickOrCross True = "\10003"
tickOrCross False = "\10007"

-- returns string representing the sequence of the norm of the given development
normSequence :: ([Trek],Bool) -> (String,Bool)
normSequence ([],_) = error "Sequence of norms not supposed to be empty"
normSequence ((x:[]),b)   | b = (showMs (norm x) ++ "\n   Yeah, the norm decreases! :)",b)
                          | otherwise = (showMs (norm x) ++ "\n   Nope :(",b)
normSequence ((x:y:xs),b) | xNorm `greater` yNorm = (showMs xNorm ++ " > " ++ fst result, snd result && True)
                          | otherwise = (showMs xNorm ++ " \8804 " ++ fst (normSequence ((y:xs),False)), False)
    where 
        xNorm = norm x
        yNorm = norm y
        result = normSequence ((y:xs),b && True)

-- returns a string with the development of the given strategy and an Int with the length of the
-- development
testStrategy :: Trek -> Int -> (String,(Int,Bool))
testStrategy (t,u) n = (header ++ sequence ++ initalDevs ++ tailDev ++ normSeq ++ unfolding, (steps,decrease))
    where
        development = develop (t,u) n
        completes = map (fst.complete.fst) development
        correctUnfolds = and (map (==(fst.fst.last) development) completes)
        unfolding = "\nUnfolding intermediates equal target of development: " ++ (tickOrCross correctUnfolds)
        steps = length development - 1
        header = "Length: " ++ show (steps) ++ "\n"
        sequence = "Sequence: " ++ (init (concat [ showPos (snd dev) ++ "," | dev <- init development ])) ++ "\n"
        initalDevs = unwords [ (showTrek (fst dev)) ++ " ->" ++ showPos (snd dev) | dev <- init development]
        tailDev = " " ++ showTrek (fst (last development)) ++ "\n"
        (normSeq,decrease) = normSequence ((map (fst) development),True)

-- returns a string with all strategies for a given Trek
testFull :: Trek -> String
testFull (t,u) = header ++ "\n" ++ defStr ++ (fst zero) ++ "\n" ++
                     inMo ++ (fst two) ++ "\n" ++
                     outMo ++ (fst four) ++ "\n" ++
                     inOrder ++ (fst one) ++ "\n" ++
                     revOrder ++ (fst three) ++ "\n" ++ header
    where
        defStr = "------------------------------------\n Default Strategy\n------------------------------------\n"
        zero = testStrategy (t,u) 0
        inOrder = "------------------------------------\n In-Order Strategy\n------------------------------------\n"
        one = testStrategy (t,u) 1
        inMo = "------------------------------------\n Innermost Strategy\n------------------------------------\n"
        two = testStrategy (t,u) 2
        revOrder = "------------------------------------\n Reversed in-Order Strategy\n------------------------------------\n"
        three = testStrategy (t,u) 3
        outMo = "------------------------------------\n Outermost Strategy\n------------------------------------\n"
        four = testStrategy (t,u) 4
        header = "\n--------------------------------------------------------------------------------------------------\n " ++ 
                show t ++ "\n--------------------------------------------------------------------------------------------------\n"
                ++ "Default: " ++ show (fst (snd zero)) 
                ++ " Innermost: " ++ show (fst (snd two)) 
                ++ " Outermost: " ++ show (fst (snd four))
                ++ " In-Order: " ++ show (fst (snd one)) 
                ++ " Reversed: " ++ show (fst (snd three)) 
                ++ " " ++ tickOrCross (snd (snd zero))
                ++ " " ++ tickOrCross (snd (snd two))
                ++ " " ++ tickOrCross (snd (snd four))
                ++ " " ++ tickOrCross (snd (snd one))
                ++ " " ++ tickOrCross (snd (snd three))

-- short version of testFull
testShort :: Trek -> String
testShort (t,u) = header 
    where
        zero = testStrategy (t,u) 0
        one = testStrategy (t,u) 1
        two = testStrategy (t,u) 2
        three = testStrategy (t,u) 3
        four = testStrategy (t,u) 4
        header = "\n--------------------------------------------------------------------------------------------------\n " ++ 
                show t ++ "\n "
                ++ "Default: " ++ show (fst (snd zero)) 
                ++ " Innermost: " ++ show (fst (snd two)) 
                ++ " Outermost: " ++ show (fst (snd four))
                ++ " In-Order: " ++ show (fst (snd one)) 
                ++ " Reversed: " ++ show (fst (snd three)) 
                ++ "\n"
                ++ " " ++ tickOrCross (snd (snd zero))
                ++ " " ++ tickOrCross (snd (snd two))
                ++ " " ++ tickOrCross (snd (snd four))
                ++ " " ++ tickOrCross (snd (snd one))
                ++ " " ++ tickOrCross (snd (snd three))
        
-- main function for the test suit
main :: IO ()
main = do
    let t1 = [parse "1.2.3.4.5"]
        t2 = t1 ++ [parse "1.2.3.4.5.6"]
        t3 = t2 ++ [parse "1.2.(3.4.5)"]
        t4 = t3 ++ [parse "1.2.3.(4.5.6)"]
        t5 = t4 ++ [parse "1.2.(3.4.5).6"]
        t6 = t5 ++ [parse "1.2.(3.4.(5.6.7)).8"]
        t7 = t6 ++ [parse "1.2.(3.4).5.6"]
        t8 = t7 ++ [parse "1.2.(3.4.5).6.7"]
        t9 = t8 ++ [parse "1.2.(3.4.(5.6.7)).8.(9.a.b)"]
        t10 = t9 ++ [parse "1.2.3.4.(5.6.7)"]
        t11 = t10 ++ [parse "1.2.3.4.5.6.7.8.9.a.(b.c.d)"]
        seq = t11
        treks = [ getFullTrek t | t <- seq ]
        result = map (testFull) treks
    putStrLn (id ("----------------------------------------\n " ++ (unwords result)))

-- helper for inductiveTest
testInductivDef :: Trek -> String
testInductivDef (t,u) = showTrek (t,u) ++ ":\n" ++ unwords tickString ++ "\n\n"
    where
        development = develop (t,u) 3
        completes = map (fst.complete.fst) development
        correctUnfolds = map (==(fst.fst.last) development) completes
        tickString = map (tickOrCross) correctUnfolds

-- tests the function 'complete' by checking if the inductively defined target of a trek
-- equals the target of a full development
inductiveTest :: IO ()
inductiveTest = do
    let t1 = [parse "1.2.3.4.5"]
        t2 = t1 ++ [parse "1.2.3.4.5.6"]
        t3 = t2 ++ [parse "1.2.(3.4.5)"]
        t4 = t3 ++ [parse "1.2.3.(4.5.6)"]
        t5 = t4 ++ [parse "1.2.(3.4.5).6"]
        t6 = t5 ++ [parse "1.2.(3.4.(5.6.7)).8"]
        t7 = t6 ++ [parse "1.2.(3.4).5.6"]
        t8 = t7 ++ [parse "1.2.(3.4.5).6.7"]
        t9 = t8 ++ [parse "1.2.(3.4.(5.6.7)).8.(9.a.b)"]
        t10 = t9 ++ [parse "1.2.3.4.(5.6.7)"]
        t11 = t10 ++ [parse "1.2.3.4.5.6.7.8.9.a.(b.c.d)"]
        seq = t11
        treks = [ getFullTrek t | t <- seq ]
        result = map (testInductivDef) treks
    putStrLn (id ("----------------------------------------\n " 
            ++ (unwords result) 
            ++ "----------------------------------------\n"))
