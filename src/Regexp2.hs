module Regexp2 where

import Data.List (sort)
import Util (dropRepeated)

-- | this encoding of regular expressions might be better
-- because it is easier to obtain a normal form in dot' and alt'
-- NF means:
-- * no directly nested Dot' in Dot'
-- * no directly nested Alt' in Alt'
-- * no Alt' [] (== zero') in Dot'
-- * argument list of Alt' is sorted without repetitions
-- (best case: disjoint elements -or- no element should be contained in another)
data RE' t
    = Atom' t
    | Dot' [RE' t]
    | Alt' [RE' t]
    | Star' (RE' t) 
    deriving (Eq, Ord)

zero' = Alt' []
one'  = Dot' []

dot' :: [RE' t] -> RE' t
dot' rs = f rs []
  where
    f [] accrs = Dot' $ reverse accrs
    f (Dot' rs': rs) accrs = f (rs' ++ rs) accrs
    f (Alt' []: rs) accrs = Alt' []
    f (r1:rs) accrs = f rs (r1:accrs)

alt' :: (Ord t) => [RE' t] -> RE' t
alt' rs = Alt' $ dropRepeated $ sort $ f rs
  where
    f [] = []
    f (Alt' rs': rs) = f (rs' ++ rs)
    f (r1:rs) = r1 : f rs

nullable' :: RE' t -> Bool
nullable' (Atom' _) = False
nullable' (Dot' rs) = and (map nullable' rs)
nullable' (Alt' rs) = or (map nullable' rs)
nullable' (Star' r) = True

derivative' :: (Eq t, Ord t) => t -> RE' t -> RE' t
derivative' t (Atom' t') =
    if t == t' then one' else zero'
derivative' t (Alt' rs) =
    alt' (map (derivative' t) rs)
derivative' t (Dot' rs) =
    let f [] = []
        f (r1 : rs) = dot' (derivative' t r1 : rs) : 
                      if nullable' r1 then f rs else []
    in  alt' (f rs)
derivative' t (Star' r) =
    dot' [derivative' t r, Star' r]
