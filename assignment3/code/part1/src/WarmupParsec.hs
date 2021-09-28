module WarmupParsec where

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

import Text.ParserCombinators.Parsec
    ( ParseError,
      digit,
      spaces,
      string,
      many1,
      (<|>),
      Parser,
      runParser,
      eof) -- exports a suitable type ParseError

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

-- Optional: if not attempted, leave as undefined
parseString :: String -> Either ParseError Exp
parseString = runParser (do e<-parserE; eof; return e) () ""

parserE =
  do
    spaces
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
  num <- many1 digit
  spaces
  return ((read :: String -> Int) num)

symbol :: String -> Parser ()
symbol s = do
  string s
  spaces
  return ()

main = print (parseString "\t \n - 1 + 2 - ( 3 ) ")