module GRegexp where

import Data.List hiding (intersect)
import qualified Data.List (intersect)
import Data.Maybe

-- | generalized regular expressions - with intersection and negation
data GRE t
    = Zero
    | One
    | Atom t
    | Dot (GRE t) (GRE t)
    | Or (GRE t) (GRE t)
    | And (GRE t) (GRE t)
    | Not (GRE t)
    | Star (GRE t)
    deriving (Eq, Ord, Show)

type Lang t = [[t]]

-- | naive implementation
naive ::(Eq t) => GRE t -> Lang t
naive Zero = []
naive One = [[]]
naive (Atom t) = [[t]]
naive (Dot r s) = [ v ++ w | v <- naive r, w <- naive s]
naive (Or r s) = union (naive r) (naive s)
naive (And r s) = Data.List.intersect (naive r) (naive s)
naive (Not r) = undefined
naive (Star r) = [] : [ v ++ w | v <- naive r, w <- naive (Star r) ]

-- drawbacks
-- * no effective element test
-- * how to implement not?
-- * easy to get repetitions
-- * efficiency?


-- | length lexicographic ordering
lleq :: (Ord t) => [t] -> [t] -> Bool
lleq xs ys =
  let lxs = length xs
      lys = length ys
  in  lxs < lys ||
      lxs == lys && xs <= ys

merge :: (Ord t) => Lang t -> Lang t -> Lang t
merge [] yss = yss
merge xss [] = xss
merge xss@(xs:xss') yss@(ys:yss')
  | xs == ys = xs : merge xss' yss'
  | lleq xs ys = xs : merge xss' yss
  | otherwise    = ys : merge xss yss'

intersect :: (Ord t) => Lang t -> Lang t -> Lang t
intersect [] yss = []
intersect xss [] = []
intersect xss@(xs:xss') yss@(ys:yss')
  | xs == ys = xs : intersect xss' yss'
  | lleq xs ys = intersect xss' yss
  | otherwise    = intersect xss yss'

difference :: (Ord t) => Lang t -> Lang t -> Lang t
difference [] yss = []
difference xss [] = xss
difference xss@(xs:xss') yss@(ys:yss')
  | xs == ys = difference xss' yss'
  | lleq xs ys = xs : difference xss' yss
  | otherwise    = difference xss yss'

sigma_star :: [t] -> Lang t
sigma_star sigma = loop
  where
    loop = [] : concatMap f loop
    snoc xs x = xs ++ [x]
    f ts = map (snoc ts) sigma

concatenate ::(Ord t) => Lang t -> Lang t -> Lang t
concatenate xss yss = collect 0
  where
    xsegs = segmentize xss
    ysegs = segmentize yss
    collect n = sort (concatMap (combine n) [0 .. n]) ++ collect (n+1)
    combine n i = concatMap (\xs -> map (\ys -> xs ++ ys) (ysegs !! (n - i))) (xsegs !! i)

xss = ["", "a", "cd"]
yss = ["", "b", "aa"]

xsegs = segmentize xss
ysegs = segmentize yss
combine n i = concatMap (\xs -> map (\ys -> xs ++ ys) (ysegs !! (n - i))) (xsegs !! i)

    
-- collect elements of the same length; returns an infinite list
segmentize :: Lang t -> [[[t]]]
segmentize = collect 0
  where
    collect n xss = takeWhile (\xs -> length xs == n) xss : collect (n+1) (dropWhile (\xs -> length xs == n) xss)

-- | declarative, but not productive
star xss = merge [[]] (concatenate xss (star xss))

-- |generate elements of the language of the gre as an ll-ascending stream
generate :: (Ord t) => [t] -> GRE t -> Lang t
generate sigma r = gen r
  where
    gen Zero = []
    gen One  = [[]]
    gen (Atom t) = [[t]]
    gen (Dot r s) = concatenate (gen r) (gen s)
    gen (Or r s) = merge (gen r) (gen s)
    gen (And r s) = intersect (gen r) (gen s)
    gen (Not r) = difference (sigma_star sigma) (gen r)
    gen (Star r) = star (gen r)

-- fix problems with concat

-- collect elements of the same length; returns a finite list for finite languages
segmentize' :: Lang t -> [[[t]]]
segmentize' = collect 0
  where
    collect n [] = []
    collect n xss = takeWhile (\xs -> length xs == n) xss : collect (n+1) (dropWhile (\xs -> length xs == n) xss)

concatenate' :: (Ord t) => Lang t -> Lang t -> Lang t
concatenate' xss yss = collect 0
  where
    xsegs = segmentize' xss
    ysegs = segmentize' yss
    exhausted xs n = all isNothing (map (maybeIndex xs) [n `div` 2 .. n])
    collect n | exhausted xsegs n && exhausted ysegs n = []
              | otherwise = (remDuplicates $ sort $ concatMap (combine n) [0 .. n]) ++ collect (n+1)
    combine n i = concatMap (\xs -> map (\ys -> xs ++ ys) (fromMaybe [] $ maybeIndex ysegs (n - i))) (fromMaybe [] $ maybeIndex xsegs i)

maybeIndex :: [a] -> Int -> Maybe a
maybeIndex [] _ = Nothing
maybeIndex (x:xs) n = if n == 0 then Just x else maybeIndex xs (n-1)

{-
xsegs = segmentize' xss
ysegs = segmentize' yss
combine n i = concatMap (\xs -> map (\ys -> xs ++ ys) (maybe [] id $ maybeIndex ysegs (n - i))) (maybe [] id $ maybeIndex xsegs i)
xhausted n = all isNothing (map (maybeIndex xsegs) [n `div` 2 .. n])
-}

-- | declarative, but not productive
star' xss = merge [[]] (concatenate' xss (star' xss))

  
-- | not generally productive
star4 :: (Ord t) => [[t]] -> [[t]]
star4 xss = collect 0
  where
    xsegs = segmentize xss
    collect n = (remDuplicates $ sort $ concatMap wordsFromPartition (partitions n)) ++ collect (n + 1)
    wordsFromPartition [] = [[]]
    wordsFromPartition (i:is) = concatMap (\w -> map (++w) (xsegs !! i)) (wordsFromPartition is)

-- | productive
star4' :: (Ord t) => [[t]] -> [[t]]
star4' xss = [] : collect 1
  where
    xsegs = segmentize xss
    infiniteResult = any (\xs -> length xs > 0) xss
    collect n 
      | infiniteResult = (remDuplicates $ sort $ concatMap wordsFromPartition (partitions n)) ++ collect (n + 1)
      | otherwise = []
    wordsFromPartition [] = [[]]
    wordsFromPartition (i:is) = concatMap (\w -> map (++w) (xsegs !! i)) (wordsFromPartition is)


-- | pn = partitions n
-- xs \in pn => sum xs = n, xi \in xs => xi > 0
-- no repetitions
partitions :: Int -> [[Int]]
partitions n
  | n == 0 = [[]]
  | otherwise = concatMap (\i -> map (i:) (partitions (n - i))) [1 .. n]

collect n = concatMap wordsFromPartition (partitions n)
wordsFromPartition [] = [[]]
wordsFromPartition (i:is) = concatMap (\w -> map (++w) (xsegs !! i)) (wordsFromPartition is)

-- | check if list is ll sorted
llsorted [] = []
llsorted [_] = []
llsorted (x:xs@(y:_)) = lleq x y : llsorted xs

-- | remove adjacent duplicates
remDuplicates :: (Eq a) => [a] -> [a]
remDuplicates [] = []
remDuplicates [x] = [x]
remDuplicates (x:xs@(y:_))
  | x == y = remDuplicates xs
  | otherwise = x : remDuplicates xs

{-
star''' sigma xss = loop
  where
    derivations = map (\t -> (derive t xss)) sigma
    mderivations = foldr merge [] derivations
    rest = concatenate' mderivations loop
    loop = [] : rest
      
derive t xss = do { xs@(x:_) <- xss; if t == x then return xs else fail "" }
-}
