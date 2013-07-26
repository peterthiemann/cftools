module Util where

-- | general utility

dropRepeated :: Eq n => [n] -> [n]
dropRepeated [] = []
dropRepeated (x:xs) = x : dropRepeated (dropWhile (==x) xs)

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f init =
    let next = f init in
    if next == init then next else fixpoint f next

-- | unused

allMaybe :: Bool -> (a -> Bool) -> Maybe [a] -> Bool
allMaybe b p Nothing = b
allMaybe b p (Just xs) = all p xs

