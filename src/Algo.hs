module Algo where

import Data.List (sort, nub, (\\))
import Data.Maybe (fromMaybe)

import Grammar
import Util

-- | emptiness check
emptyLanguage :: (Ord n) => CFG n t -> n -> Bool
emptyLanguage (CFG nts ts ps _) start = 
    not (start `elem` usefulNts ps [])

usefulNts :: (Ord n) => [Production n t] -> [n] -> [n]
usefulNts ps init = 
    fixpoint (usefulStep ps) init

usefulStep :: Ord n => [Production n t] -> [n] -> [n]
usefulStep ps assumed =
    dropRepeated $ sort ([nt | p@(Production nt alpha) <- ps, usefulProduction p assumed] ++ assumed)

usefulProduction :: Eq n => Production n t -> [n] -> Bool
usefulProduction (Production nt alpha) assumed =
    all (usefulSymbol assumed) alpha

usefulSymbol :: (Eq n) => [n] -> Symbol n t -> Bool
usefulSymbol assumed (Right t) = True
usefulSymbol assumed (Left n)  = n `elem` assumed

-- | nullable check -- does L contain the empty word?
nullableLanguage :: (Ord n) => CFG n t -> n -> Bool
nullableLanguage (CFG _ _ ps _) start =
    start `elem` nullableNts ps []

nullableNts :: (Ord n) => [Production n t] -> [n] -> [n]
nullableNts ps init =
    fixpoint (nullableStep ps) init

nullableStep :: Ord n => [Production n t] -> [n] -> [n]
nullableStep ps assumed =
    dropRepeated $ sort ([nt | Production nt alpha <- ps, all (nullableSymbol assumed) alpha] ++ assumed)

nullableSymbol :: (Eq n) => [n] -> Symbol n t -> Bool
nullableSymbol assumed (Right t) = False
nullableSymbol assumed (Left n)  = n `elem` assumed

-- | reduce -- keep only productions for useful NTs
reduceGrammar :: (Eq n) => CFG n t -> [n] -> CFG n t
reduceGrammar (CFG nts ts ps start) usefulNts =
    CFG nts ts ps' start
    where
    ps' = [Production nt alpha | Production nt alpha <- ps, all (usefulSymbol usefulNts) alpha]

-- | package a reduced CFG with useful NTs and nullable NTs
mkRCFG :: (Ord n) => CFG n t -> RCFG n t
mkRCFG cfg@ (CFG _ _ ps _) = 
    let useful = usefulNts ps []
        redcfg@ (CFG _ _ redps _) = reduceGrammar cfg useful
    in
    RCFG redcfg useful (nullableNts redps [])

-- | reduce productions with respect to an existing reduced grammar
-- yields the remaining useful productions, the useful nonterminals, and
-- the nullable nonterminals
reduceProductions :: (Ord n) =>
                     RCFG n t -> [Production n t] -> ([Production n t], [n], [n])
reduceProductions rcfg ps =
    let nUseful = usefulNts ps (useful rcfg)
        nPs = [p | p <- ps, usefulProduction p nUseful]
        nNullable = nullableNts nPs (nullable rcfg) 
    in (nPs, nUseful, nNullable)

-- | a substitution is actually a renaming of nonterminal symbols, represented by a list of pairs
type Substitution n = [(n, n)]
dom :: Substitution n -> [n]
dom = map fst
ran :: Substitution n -> [n]
ran = map snd
app :: (Eq n) => Substitution n -> n -> n
app s n = fromMaybe n (lookup n s)
ext :: Substitution n -> n -> n -> Substitution n
ext s n n' = (n, n') : s
emptysub :: Substitution n
emptysub = []

-- | apply a substitution to a symbol
appSym :: (Eq n) => Substitution n -> Symbol n t -> Symbol n t
appSym s = either (Left . app s) Right

-- | match new productions with new lhs nonterminals to existing productions
-- cannot do this one at a time because productions may be mutually recursive
-- result is a substitution S of nonterminals: 
-- * new productions can be removed for each n in dom(S)
-- * S must be applied to the right hand sides of the new productions
-- * S must be applied to the new start symbol
matchProductions :: (Eq n, Eq t) => RCFG n t -> [Production n t] -> Substitution n
matchProductions rcfg ps =
    head (findMatchProductions emptysub originals ps [] ++ [emptysub])
    where CFG _ _ originals _ = cfg rcfg

findMatchProductions :: (Eq n, Eq t)
          => Substitution n -> [Production n t] -> [Production n t]
          -> [Production n t]
          -> [Substitution n]
findMatchProductions sub originals [] ps =
    let nts_orig = [n | Production n _ <- originals]
        nts_new  = [n | Production n _ <- ps]
    in  if any (`elem` dom sub) nts_new -- any nonterminal only partially matched?
        || any (`elem` ran sub) nts_orig
        then []
        else [sub]
findMatchProductions sub originals (p : ps) acc_ps =
    do (sub', remaining) <- findMatchProd sub originals p
       findMatchProductions sub' remaining ps acc_ps
    ++ findMatchProductions sub originals ps (p : acc_ps)
       

findMatchProd :: (Eq n, Eq t)
          => Substitution n -> [Production n t] -> Production n t
          -> [(Substitution n, [Production n t])]
findMatchProd sub [] new =
    []
findMatchProd sub (p@(Production n alpha) : rest) p'@(Production n' alpha') =
    (map (\s -> (s, rest)) $
    if app sub n' == n
    then findMatch sub alpha alpha'
    else findMatch (ext sub n' n) alpha alpha')
    ++
    (map (\(s, rem) -> (s, p : rem)) $ findMatchProd sub rest p')

findMatch :: (Eq n, Eq t)
          => Substitution n -> [Symbol n t] -> [Symbol n t] -> [Substitution n]
findMatch sub [] [] =
    [sub]
findMatch sub (Right t1 : alpha1) (Right t2 : alpha2) =
    if t1 == t2 
    then findMatch sub alpha1 alpha2
    else []
findMatch sub (Left n1 : alpha1) (Left n2 : alpha2) =
    if app sub n2 == n1 
    then findMatch sub alpha1 alpha2
    else if n2 `elem` dom sub
    then []
    else findMatch (ext sub n2 n1) alpha1 alpha2
findMatch sub _ _ =
    []

derivative :: (Ord n, Ord t) => RCFG (n,[t]) t -> t
           -> RCFG (n,[t]) t
derivative rcfg t =
    let (newNts, newPs, newStart) = derivativeProductions rcfg t
        (newPs', newUseful, newNullable) = reduceProductions rcfg newPs
        sub = matchProductions rcfg newPs'
        remainingNewPs = [ Production n (map (appSym sub) alpha)
                         | Production n alpha <- newPs'
                         , not (n `elem` dom sub)]
        remainingUseful = [n | n <- newUseful, not (n `elem` dom sub)]
        remainingNullable  = [n | n <- newNullable, not (n `elem` dom sub)]
        substitutedStart = app sub newStart
        remainingNts = nub (substitutedStart : nts ++ remainingUseful)
        CFG nts ts ps start = cfg rcfg
    in  RCFG { cfg = CFG remainingNts ts (ps ++ remainingNewPs) substitutedStart
             , useful = nub (useful rcfg ++ remainingUseful)
             , nullable = nub (nullable rcfg ++ remainingNullable)}

-- | derivative -- should better be done with a proper monad...
derivativeProductions :: (Eq n, Eq t) => RCFG (n,[t]) t -> t
                      -> ([(n, [t])], [Production (n, [t]) t], (n, [t]))
derivativeProductions (RCFG cfg@(CFG nts ts ps start) useful nullable) t =
    (newNTs, newPs, deriveNT start t)
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

