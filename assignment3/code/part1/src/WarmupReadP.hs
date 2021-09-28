module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   E::= TE'| "-"TE'
--   E'::="+"TE'|"-"TE'| ϵ
--   T::=num| "(" E ")"

import Control.Applicative ((<|>))
import Data.Char ( isNumber )
import Text.ParserCombinators.ReadP
    ( ReadP, eof, many1, readP_to_S, satisfy, skipSpaces, string )

-- The declaration of ReadP
-- newtype ReadP a = R (forall b . (a -> P b) -> P b)
-- It may equals newtype ReadP a = R ( (a -> P b) -> P b)

--  ReadP string -》 ReadP Either ParseError Exp

-- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a -- may use synomym for easier portability to Parsec

type ParseError = String -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

parseString :: String -> Either ParseError Exp
parseString s = case readP_to_S (do e <- parserE; eof; return e) s of
  [] -> Left "ParseError!"
  [(a, _)] -> Right a
  _ -> Left "TOO Many OUTPUT!"

parserE :: Parser Exp
parserE =
  do
    skipSpaces
    symbol "-"
    e <- parserT
    parserE' (Negate e)
    <|> do
      e <- parserT
      parserE' e

parserE' :: Exp -> Parser Exp
parserE' exp =
  do
    symbol "+"
    e <- parserT
    parserE' (Add exp e)
    <|> do
      symbol "-"
      e <- parserT
      parserE' (Add exp (Negate e))
    <|> return exp

parserT :: Parser Exp
parserT =
  Num <$> pNum
    <|> do
      symbol "("
      e <- parserE
      symbol ")"
      return e

pNum :: Parser Int
pNum = do
  num <- many1 (satisfy isNumber)
  skipSpaces
  return ((read :: String -> Int) num)

symbol :: String -> Parser ()
symbol s = do
  string s
  skipSpaces
  return ()

-- main = print (parseString "( - ( - 123 ) - ( 69 + 1 ) ) + 5")