module Regexp.Parser
( Alphabet
, regexp
, parseRegexp
) where

import           Prelude hiding (foldr1) -- GHC 7.8 compat

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Foldable (foldr1) -- GHC 7.8 compat
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)

import           Regexp

type Alphabet t = [t]

-------------------------------------------------------------------------------
-- Parsing data structure

data Prec3 t
    = PZero
    | POne
    | PAtom t
    | Parens (Prec0 t)
  deriving (Read, Show, Eq, Ord)

data Prec2 t
    = Prec3 (Prec3 t)
    | PStar (Prec3 t)
  deriving (Read, Show, Eq, Ord)

data Prec1 t = PDot (NonEmpty (Prec2 t))
  deriving (Read, Show, Eq, Ord)

data Prec0 t = PAlt (NonEmpty (Prec1 t))
  deriving (Read, Show, Eq, Ord)

-------------------------------------------------------------------------------
-- Parsers

between :: Parser left -> Parser right -> Parser middle -> Parser middle
between left right middle = (left *> middle) <* right

nonEmptyInfixList :: Parser a -> Parser op -> Parser (NonEmpty a)
nonEmptyInfixList elemParser opParser
    = (\(x:xs) -> x:|xs) <$> elemParser `sepBy1` opParser

literal :: Parser a -> Parser a
literal p = p <* skipSpace

literalChar :: Char -> Parser Char
literalChar = literal . char

zeroL, oneL, lparenL, rparenL, starL, dotL, plusL :: Parser Char
zeroL   = literalChar 'O'
oneL    = literalChar 'E'
lparenL = literalChar '('
rparenL = literalChar ')'
starL   = literalChar '*'
dotL    = literalChar '.'
plusL   = literalChar '+'

-- Note that @zero@, @one@ and @parens@ may clash with @atom@ for some
-- alphabets.
prec3 :: Alphabet (Parser t) -> Parser (Prec3 t)
prec3 sigma = zero <|> one <|> atom <|> parens
  where
    zero   = zeroL *> pure PZero
    one    = oneL *> pure POne
    atom   = literal (PAtom <$> choice sigma)
    parens = Parens <$> between lparenL rparenL (prec0 sigma)

prec2 :: Alphabet (Parser t) -> Parser (Prec2 t)
prec2 sigma = do
    r <- prec3 sigma
    starL *> pure (PStar r) <|> pure (Prec3 r)

prec1 :: Alphabet (Parser t) -> Parser (Prec1 t)
prec1 sigma = PDot <$> nonEmptyInfixList (prec2 sigma) dotL

prec0 :: Alphabet (Parser t) -> Parser (Prec0 t)
prec0 sigma = PAlt <$> nonEmptyInfixList (prec1 sigma) plusL

-------------------------------------------------------------------------------
-- Conversion to AST

toRE3 :: Prec3 t -> RE t
toRE3 PZero = Zero
toRE3 POne = One
toRE3 (PAtom a) = Atom a
toRE3 (Parens r) = toRE0 r

toRE2 :: Prec2 t -> RE t
toRE2 (Prec3 r) = toRE3 r
toRE2 (PStar r) = Star (toRE3 r)

toRE1 :: Prec1 t -> RE t
toRE1 (PDot rs) = foldr1 Dot $ fmap toRE2 rs

toRE0 :: Prec0 t -> RE t
toRE0 (PAlt rs) = foldr1 Alt $ fmap toRE1 rs

-------------------------------------------------------------------------------
-- Convenience

regexp :: Alphabet (Parser t) -> Parser (RE t)
regexp sigma = toRE0 <$> prec0 sigma

parseRegexp :: Alphabet Char -> Text -> Either String (RE Char)
parseRegexp sigma
    = parseOnly (skipSpace *> (regexp sigmaParsers <* endOfInput))
  where
    sigmaParsers = map char sigma
