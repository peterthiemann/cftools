import Data.List (sort)

-- representation of context-free grammar

type Symbol n t = Either n t

data Production n t = Production n [Symbol n t]

data CFG n t
    = CFG [n] [t] [Production n t] n

data RCFG n t
    = RCFG (CFG n t) [n] [n]

-- | a reduced CFG packaged with useful NTs and nullable NTs
mkRCFG :: CFG n t -> RCFG n t
mkRCFG cfg = 
    let useful = usefulNts cfg
        redcfg = reduceGrammar cfg useful
    in
    RCFG redcfg useful (nullableNts cfg')

-- | emptiness check
emptyLanguage :: (Ord n) => CFG n t -> n -> Bool
emptyLanguage cfg start = 
    not (start `elem` usefulNts cfg)

usefulNts :: (Ord n) => CFG n t -> [n]
usefulNts cfg = 
    fixpoint (useful cfg) []

useful :: Ord n => CFG n t -> [n] -> [n]
useful (CFG nts ts ps _) assumed =
    dropRepeated $ sort [nt | Production nt alpha <- ps, all (usefulSymbol assumed) alpha]

usefulSymbol :: (Eq n) => [n] -> Symbol n t -> Bool
usefulSymbol assumed (Right t) = True
usefulSymbol assumed (Left n)  = n `elem` assumed

-- | nullable check -- does L contain the empty word?
nullableLanguage :: (Ord n) => CFG n t -> n -> Bool
nullableLanguage cfg start =
    start `elem` nullableNts cfg

nullableNts :: (Ord n) => CFG n t -> [n]
nullableNts cfg =
    fixpoint (nullable cfg) []

nullable :: Ord n => CFG n t -> [n] -> [n]
nullable (CFG nts ts ps _) assumed =
    dropRepeated $ sort [nt | Production nt alpha <- ps, all (nullableSymbol assumed) alpha]

nullableSymbol :: (Eq n) => [n] -> Symbol n t -> Bool
nullableSymbol assumed (Right t) = False
nullableSymbol assumed (Left n)  = n `elem` assumed

-- | reduce -- keep only productions for useful NTs
reduceGrammar :: CFG n t -> [n] -> CFG n t
reduceGrammar (CFG nts ts ps start) usefulNts =
    CFG nts ts ps' start
    where
    ps' = [Production nt alpha | all (usefulSymbol usefulNts) alpha]

-- | general utility

dropRepeated :: Eq n => [n] -> [n]
dropRepeated [] = []
dropRepeated (x:xs) = x : dropRepeated (dropWhile (==x) xs)

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f init =
    let next = f init in
    if next == init then next else fixpoint f next

-- | example grammars

ex0 = CFG ["S", "A"] [] [Production "S" [], Production "S" [Left "A"], Production "A" [Left "A"]] "S"

-- | simple expressions, left recursive
ex1 = CFG
      ["F", "T"]
      ["x", "+"]
      [Production "T" [Left "F"]
      ,Production "T" [Left "T", Right "+", Left "F"]
      ,Production "F" [Right "x"]]  
      "F"
