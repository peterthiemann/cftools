module Equivalence where

import Regexp

equiv :: Ord t => [t] -> RE t -> RE t -> Bool
equiv sigma r s = rewrite sigma [] [(r, s)]

rewrite :: Ord t => [t] -> [(RE t, RE t)] -> [(RE t, RE t)] -> Bool
rewrite sigma seen [] = True
rewrite sigma seen (p : worklist) | p `elem` seen = 
        rewrite sigma seen worklist
rewrite sigma seen (p@(r, s) : worklist) =
        (nullable r == nullable s) &&
        rewrite sigma (p : seen) ([(derivative t r, derivative t s) | t <- sigma] ++ worklist)

examples = [
         (Zero, One, False),
         (Atom 'a', Atom 'a', True),
         (Atom 'b', Atom 'c', False),
         (Star (Atom 'a'), Alt One (Dot (Atom 'a') (Star (Atom 'a'))), True), -- a* = 1+aa*
         (Star (Alt (Atom 'a') (Atom 'b')), Star (Dot (Star (Atom 'a')) (Star (Atom 'b'))), True) -- (a+b)* = (a*b*)*
         ]

runExamples :: Ord t => [t] -> [(RE t, RE t, Bool)] -> Bool
runExamples sigma [] = True
runExamples sigma ((r,s,result) : rest) = 
            (equiv sigma r s == result) && runExamples sigma rest

test = runExamples "abc" examples