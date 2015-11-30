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
         (Star (Atom 'a'), Alt One (Dot (Atom 'a') (Star (Atom 'b'))), False), -- a* != 1+ab*
         (Star (Alt (Atom 'a') (Atom 'b')), Star (Dot (Star (Atom 'a')) (Star (Atom 'b'))), True) -- (a+b)* = (a*b*)*
         ]

runExamples :: Ord t => [t] -> [(RE t, RE t, Bool)] -> Bool
runExamples sigma [] = True
runExamples sigma ((r,s,result) : rest) = 
            (equiv sigma r s == result) && runExamples sigma rest

test = runExamples "abc" examples

-- with counterexample

data Result t = Yes | No [t]
     deriving   Show

equivc :: Ord t => [t] -> RE t -> RE t -> Result t
equivc sigma r s = rewritec sigma [] [(r, s)] [[]]

rewritec :: Ord t => [t] -> [(RE t, RE t)] -> [(RE t, RE t)] -> [[t]] -> Result t
rewritec sigma seen [] [] = Yes
rewritec sigma seen (p : worklist) (path : rest) | p `elem` seen = 
         rewritec sigma seen worklist rest
rewritec sigma seen (p@(r, s) : worklist) (path : rest) =
         if nullable r /= nullable s
         then No path
         else rewritec sigma (p : seen)
                             ([(derivative t r, derivative t s) | t <- sigma] ++ worklist)
                             (map (: path) sigma ++ rest)

runExamplesc :: Ord t => [t] -> [(RE t, RE t, Bool)] -> [Result t]
runExamplesc sigma [] = []
runExamplesc sigma ((r,s,result) : rest) = 
            equivc sigma r s : runExamplesc sigma rest

testc = runExamplesc "abc" examples
