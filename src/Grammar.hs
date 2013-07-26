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
    = RCFG { cfg :: CFG n t, useful :: [n], nullable :: [n] }
      deriving Show

-- | a reduced CFG packaged with useful NTs and nullable NTs
mkRCFG :: (Ord n) => CFG n t -> RCFG n t
mkRCFG cfg@ (CFG _ _ ps _) = 
    let useful = usefulNts ps
        redcfg = reduceGrammar cfg useful
    in
    RCFG redcfg useful (nullableNts redcfg)

-- | emptiness check
emptyLanguage :: (Ord n) => CFG n t -> n -> Bool
emptyLanguage (CFG nts ts ps _) start = 
    not (start `elem` usefulNts ps)

usefulNts :: (Ord n) => [Production n t] -> [n]
usefulNts ps = 
    fixpoint (usefulStep ps) []

usefulStep :: Ord n => [Production n t] -> [n] -> [n]
usefulStep ps assumed =
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
    CFG (newNTs ++ nts) ts (newPs ++ ps) (deriveNT start t)
    where
    (newNTs, newPs) = worker [start] [] ([], [])
    -- NTs to consider, NTs already processed, (accumlated nts and productions)
    worker [] processed nps =
        nps
    worker nts0@(nt:nts) processed (nts', ps') = 
        let ntprods = [Production n alpha | Production n alpha <- ps, n == nt] 
            newNT   = deriveNT nt t
            (newprods, candidates) = processProductions newNT ntprods [] []
            considerNTs = (nub candidates \\ processed) \\ nts0
        in
        worker (considerNTs ++ nts) (nt:processed) (newNT : nts', newprods ++ ps')

    -- 
    processProductions newNT [] newprods candidates =
        (newprods, candidates)
    processProductions newNT (Production nt alpha:ps) newprods candidates =
        let (rhss, candidates') = deriveAlpha alpha in
        processProductions newNT ps ([Production newNT alpha' | alpha' <- rhss] ++ newprods)
                               (candidates' ++ candidates)

    deriveAlpha [] =
        ([], [])
    deriveAlpha (Left nt:rest) = 
        if nt `elem` nullable
        then let (rhss, candidates) = deriveAlpha rest in
             ((Left (deriveNT nt t):rest) : rhss, nt : candidates)
        else ([Left (deriveNT nt t):rest], [nt])
    deriveAlpha (Right t':rest) =
        if t==t'
        then ([rest], [])
        else ([], [])

    deriveNT (n, ts) t =
        (n, t:ts)

-- | attempt to match new productions with existing ones,
-- on success generate a substitution : new nts -> old nts
matchProductions (CFG nts ts ps start) newNTs newPs =
    undefined

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
