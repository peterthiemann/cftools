import Data.List (sort, nub, (\\))

-- representation of context-free grammar

type Symbol n t = Either n t

data Production n t
    = Production n [Symbol n t]
      deriving Show

data CFG n t
    = CFG [n] [t] [Production n t] n
      deriving Show

data RCFG n t
    = RCFG (CFG n t) [n] [n]
      deriving Show

-- | a reduced CFG packaged with useful NTs and nullable NTs
mkRCFG :: (Ord n) => CFG n t -> RCFG n t
mkRCFG cfg = 
    let useful = usefulNts cfg
        redcfg = reduceGrammar cfg useful
    in
    RCFG redcfg useful (nullableNts redcfg)

-- | emptiness check
emptyLanguage :: (Ord n) => CFG n t -> n -> Bool
emptyLanguage cfg start = 
    not (start `elem` usefulNts cfg)

usefulNts :: (Ord n) => CFG n t -> [n]
usefulNts cfg = 
    fixpoint (usefulStep cfg) []

usefulStep :: Ord n => CFG n t -> [n] -> [n]
usefulStep (CFG nts ts ps _) assumed =
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
    fixpoint (nullableStep cfg) []

nullableStep :: Ord n => CFG n t -> [n] -> [n]
nullableStep (CFG nts ts ps _) assumed =
    dropRepeated $ sort [nt | Production nt alpha <- ps, all (nullableSymbol assumed) alpha]

nullableSymbol :: (Eq n) => [n] -> Symbol n t -> Bool
nullableSymbol assumed (Right t) = False
nullableSymbol assumed (Left n)  = n `elem` assumed

-- | reduce -- keep only productions for useful NTs
reduceGrammar :: (Eq n) => CFG n t -> [n] -> CFG n t
reduceGrammar (CFG nts ts ps start) usefulNts =
    CFG nts ts ps' start
    where
    ps' = [Production nt alpha | Production nt alpha <- ps, all (usefulSymbol usefulNts) alpha]



-- | derivative -- should better be done with a proper monad...
derivative :: (Eq n, Eq t) => RCFG (n,[t]) t -> t -> CFG (n,[t]) t
derivative (RCFG cfg@(CFG nts ts ps start) useful nullable) t =
    worker [start] [] cfg
    where
    -- NTs to consider, NTs already processed, current cfg (where nts and productions are added)
    worker [] processed (CFG nts' ts' ps' start') =
        (CFG nts' ts' ps' (deriveNT start t))
    worker nts0@(nt:nts) processed (CFG nts' ts' ps' start') = 
        let ntprods = [Production n alpha | Production n alpha <- ps, n == nt] 
            (newprods, newNTs) = processProductions ntprods [] []
            considerNTs = (nub newNTs \\ processed) \\ nts0
        in
        worker (considerNTs ++ nts) (nt:processed) (CFG (deriveNT nt t : nts') ts' (newprods ++ ps') start')

    processProductions [] newprods newNTs =
        (newprods, newNTs)
    processProductions (Production nt alpha:ps) newprods newNTs =
        let (rhss, newNTs') = deriveAlpha alpha newNTs in
        processProductions ps ([Production (deriveNT nt t) alpha' | alpha' <- rhss] ++ newprods) newNTs'

    deriveAlpha [] newNTs = ([], newNTs)
    deriveAlpha (Left nt:rest) newNTs = 
        if nt `elem` nullable
        then let (rhss, newNTs') = deriveAlpha rest newNTs in
             ((Left (deriveNT nt t):rest) : rhss, deriveNT nt t : newNTs')
        else ([Left (deriveNT nt t):rest], deriveNT nt t : newNTs)
    deriveAlpha (Right t':rest) newNTs =
        if t==t'
        then ([rest], newNTs)
        else ([], newNTs)

    deriveNT (n, ts) t = (n, t:ts)


-- | general utility

dropRepeated :: Eq n => [n] -> [n]
dropRepeated [] = []
dropRepeated (x:xs) = x : dropRepeated (dropWhile (==x) xs)

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f init =
    let next = f init in
    if next == init then next else fixpoint f next

liftG :: CFG n t -> CFG (n,[t]) t
liftG (CFG nts ts ps start) =
    CFG (map liftNT nts) ts (map liftProd ps) (liftNT start)
    where
    liftNT n = (n,[])
    liftProd (Production n alpha) = Production (liftNT n) (map liftSymbol alpha)
    liftSymbol (Left n)  = Left (liftNT n)
    liftSymbol (Right t) = Right t

-- | unused

allMaybe :: Bool -> (a -> Bool) -> Maybe [a] -> Bool
allMaybe b p Nothing = b
allMaybe b p (Just xs) = all p xs

-- | example grammars

ex0 = CFG
      ['S', 'A']
      []
      [Production 'S' []
      ,Production 'S' [Left 'A']
      ,Production 'A' [Left 'A']]
      'S'

-- | simple expressions, left recursive
ex1 = CFG
      ['F', 'T']
      ['x', '+']
      [Production 'T' [Left 'F']
      ,Production 'T' [Left 'T', Right '+', Left 'F']
      ,Production 'F' [Right 'x']]  
      'T'

ex2 = CFG
      ['F']
      ['x', 'y']
      [Production 'F' [Right 'x']
      ,Production 'F' [Right 'y']]  
      'F'

ex3 = CFG
      ['F']
      ['x', 'y']
      [Production 'F' []
      ,Production 'F' [Right 'y', Left 'F']]  
      'F'

ex4 = CFG
      ['F']
      ['x', 'y']
      [Production 'F' []
      ,Production 'F' [Left 'F', Right 'y']]  
      'F'
