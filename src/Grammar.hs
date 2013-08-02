module Grammar where

-- representation of context-free grammar

type Symbol n t
    = Either n t

data NT n t
    = NT [t] n [t]
    deriving (Eq, Ord)

instance (Show n, Show t) => Show (NT n t) where
    showsPrec _ (NT lts n rts) =
        shows lts . 
        showChar ':' .
        shows n .
        showChar ':' .
        shows rts

data Production n t
    = Production n [Symbol n t]
      -- deriving Show

data CFG n t
    = CFG { nts :: [n], ts :: [t], ps :: [Production n t], start :: n }
      deriving Show

data RCFG n t
    = RCFG { cfg :: CFG n t, useful :: [n], nullable :: [n] }
      deriving Show

rstart :: RCFG n t -> n
rstart = start . cfg

remptyLanguage :: (Eq n) => RCFG n t -> Bool
remptyLanguage rcfg = not (rstart rcfg `elem` useful rcfg)

rnullableLanguage :: (Eq n) => RCFG n t -> Bool
rnullableLanguage rcfg = rstart rcfg `elem` nullable rcfg

liftG :: CFG n t -> CFG (NT n t) t
liftG (CFG nts ts ps start) =
    CFG (map liftNT nts) ts (map liftProd ps) (liftNT start)

liftNT n = NT [] n []
liftProd (Production n alpha) = Production (liftNT n) (map liftSymbol alpha)
liftSymbol (Left n)  = Left (liftNT n)
liftSymbol (Right t) = Right t

liftRG :: RCFG n t -> RCFG (NT n t) t
liftRG rcfg =
    RCFG { cfg = liftG (cfg rcfg)
         , useful = map liftNT (useful rcfg)
         , nullable = map liftNT (nullable rcfg) }

--

instance (Show n, Show t) => Show (Production n t) where
  showsPrec _ (Production n alpha) =
      showString " { " .
      shows n .
      showString " -> " .
      foldr (.) id (map showSymbol alpha) .
      showString " } "

showSymbol :: (Show n, Show t) => Symbol n t -> ShowS
showSymbol (Left n) =
    shows n
showSymbol (Right t) =
    shows t


showGrammar :: CFG (NT Char Char) Char -> ShowS
showGrammar (CFG nts ts ps start) =
    showString "(CFG " .
    shows nts .
    showChar ' ' .
    shows ts . 
    showString " { " .
    foldr (.) id (map showProduction ps) .
    showString " } " .
    showNT start .
    showChar ')'

showProduction :: Production (NT Char Char) Char -> ShowS
showProduction (Production n alpha) =
    showNT n .
    showString " -> " .
    foldr (.) id (map showCharSymbol alpha) .
    showString ", "

showNT :: NT Char Char -> ShowS
showNT (NT [] n []) =
    showChar n
showNT (NT [] n rts) =
    showString (n : ':' : rts)
showNT (NT lts n []) =
    showString (lts ++ ':' : [n])
showNT (NT lts n rts) =
    showString (lts ++  ':' : n : ':' : rts)

showCharSymbol :: Symbol (NT Char Char) Char -> ShowS
showCharSymbol (Left nt) =
    showNT nt . showChar ' '
showCharSymbol (Right c) =
    showChar c . showChar ' '
