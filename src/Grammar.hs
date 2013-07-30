module Grammar where

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

liftG :: CFG n t -> CFG (n,[t]) t
liftG (CFG nts ts ps start) =
    CFG (map liftNT nts) ts (map liftProd ps) (liftNT start)

liftNT n = (n,[])
liftProd (Production n alpha) = Production (liftNT n) (map liftSymbol alpha)
liftSymbol (Left n)  = Left (liftNT n)
liftSymbol (Right t) = Right t

liftRG :: RCFG n t -> RCFG (n, [t]) t
liftRG rcfg =
    RCFG { cfg = liftG (cfg rcfg), useful = map liftNT (useful rcfg), nullable = map liftNT (nullable rcfg) }

    