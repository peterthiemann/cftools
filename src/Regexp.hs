module Regexp where

data RE t
    = Zero
    | One
    | Atom t
    | Dot (RE t) (RE t)
    | Alt (RE t) (RE t)
    | Star (RE t)
    deriving (Eq, Ord, Show)

nullable :: RE t -> Bool
nullable Zero = False
nullable One  = True
nullable (Atom _) = False
nullable (Dot r1 r2) = nullable r1 && nullable r2
nullable (Alt r1 r2) = nullable r1 || nullable r2
nullable (Star r) = True

dot :: RE t -> RE t -> RE t
dot Zero r = Zero
dot r Zero = Zero
dot One r = r
dot r One = r
dot (Dot r11 r12) r2 = Dot r11 (dot r12 r2)
dot r1 r2 = Dot r1 r2

alt :: (Ord t) => RE t -> RE t -> RE t
alt r1 r2 | contained r1 r2 = r2
alt r1 r2 | contained r2 r1 = r1
alt r1 r2 | r1 < r2 = Alt r1 r2
          | r1 == r2 = r1
          | otherwise = Alt r2 r1

contained :: (Ord t) => RE t -> RE t -> Bool
contained Zero r2 = True
contained One r2 = nullable r2
contained (Atom t) r2 = nullable (derivative t r2)
contained r1@(Dot _ _) r2 = False
contained (Alt r11 r12) r2 = contained r11 r2 && contained r12 r2
contained r1@(Star _) r2 = False

star Zero = One
star One  = One
star (Star r) = Star r
star r = Star r

derivative :: (Ord t) => t -> RE t -> RE t
derivative t Zero =
    Zero
derivative t  One =
    Zero
derivative t (Atom t') =
    if t == t' then One else Zero
derivative t (Alt r1 r2) = 
    derivative t r1 `alt` derivative t r2
derivative t (Dot r1 r2) =
    dot (derivative t r1) r2 `alt` if nullable r1 then derivative t r2 else Zero
derivative t (Star r) =
    derivative t r `dot` star r

-- | split a regexp r in two such that L(r) = L (r1) L (r2)
split :: RE t -> [(RE t, RE t)]
split Zero = [(Zero, Zero)]
split One = [(One, One)]
split r@(Atom t) = [(One, r), (r, One)]
split r@(Dot r1 r2) =
    [ (r11, r12 `dot` r2) | (r11, r12) <- split r1] ++
    [ (r1 `dot` r21, r22) | (r21, r22) <- tail $ split r2]
split r@ (Alt r1 r2) = 
    [(One, r), (r, One)]
split r@ (Star r1) =
    [(One, r), (Star r1, Star r1), (r, One)]
