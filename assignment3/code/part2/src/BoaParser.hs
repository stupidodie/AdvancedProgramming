module BoaParser (ParseError, parseString) where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import BoaAST
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum, isPrint)
import Data.List

type Parser a = ReadP a

type ParseError = String

parseString :: String -> Either ParseError Program
parseString s =
   case readP_to_S (do whitespace; comments; r <- program; eof; return r) s of
                [] -> Left "Cannot parse."
                [(a,_)] -> Right a
                _ -> error "Ambiguous grammar"

program :: Parser Program
program = stmts

stmts :: Parser [Stmt]
stmts = do
        n <- stmt
        symbol ";"
        n1 <- stmts
        return (n:n1)
       <|>
        do
        n <- stmt
        return [n]

stmt :: Parser Stmt
stmt = do
       name <- ident
       symbol "="
       SDef name <$> expp
       <|>
       do
       SExp <$> expp

-- Disambiguated grammar (only expressions and operators has been changed)

-- First level of precedence with 'not'
expp :: Parser Exp
expp = do
       keyword "not"
       Not <$> expp
       <|>
       do
         e1 <- e
         expp' e1

-- Second level of precdence with relational operators. Notice that they are
-- non-associative and therefore the function is not recursive.
expp' :: Exp -> Parser Exp
expp' e1 = do {symbol "=="; Oper Eq e1 <$> e}
        <|>
        do {symbol "!="; Not . Oper Eq e1 <$> e}
        <|>
        do {symbol "<"; Oper Less e1 <$> e}
        <|>
        do {symbol "<="; Not . Oper Greater e1 <$> e;}
        <|>
        do {symbol ">"; Oper Greater e1 <$> e}
        <|>
        do {symbol ">="; Not . Oper Less e1 <$> e}
        <|>
        do {satisfy isSpace ; keyword "in"; Oper In e1 <$> e}
        <|>
        do {keyword "not"; keyword "in"; Not . Oper In e1 <$> e}
        <|>
        do
          return e1

-- Third level of precedence where plus and minus is handled. These operators
-- are left-associative. Grammar is split into e and e' to avoid left-recursion.
e :: Parser Exp
e = do
      _t <- t
      e' _t

e' :: Exp -> Parser Exp
e' e1 = do
          symbol "+";
          _t <- t;
          e' (Oper Plus e1 _t)
        <|>
        do
          symbol "-";
          _t <- t;
          e' (Oper Minus e1 _t)
        <|>
          return e1

-- Fourth level of precedence, here mul, div and mod is handled.
t :: Parser Exp
t = do
      _f <- f
      t' _f

t' :: Exp -> Parser Exp
t' t1 = do
          symbol "*";
          _f <- f;
          t' (Oper Times t1 _f)
        <|>
        do
          symbol "//";
          _f <- f;
          t' (Oper Div t1 _f)
        <|>
        do
          symbol "%";
          _f <- f;
          t' (Oper Mod t1 _f)
        <|>
          return t1

-- Note that <++ is used to bias the parser towards the left choice, speeding
-- things up.
f :: Parser Exp
f = do
      Var <$> ident
    <|>
      numConst
    <|>
      stringConst
    <|>
    do
      symbol "None"
      return (Const NoneVal)
    <|>
    do
      symbol "False"
      return (Const FalseVal)
    <|>
    do
      symbol "True"
      return (Const TrueVal)
    <|>
    do
      symbol "("
      e1 <- expp
      symbol ")"
      return e1
    <|>
    do
      symbol "["
      es <- expz
      symbol "]"
      return (List es)
    <++
    do
      symbol "["
      char '('
      e1 <- expp
      char ')'
      f <- forc
      cs <- clausez [f]
      symbol "]"
      return (Compr e1 cs)
    <++
    do
      symbol "["
      e1 <- expp
      satisfy isSpace
      f <- forc
      cs <- clausez [f]
      symbol "]"
      return (Compr e1 cs)
    <++
    do
      i <- ident
      symbol "("
      e <- expz
      symbol ")"
      return (Call i e)


forc :: Parser CClause
forc = do
      keyword "for"
      i <- ident
      keyword "in"
      CCFor i <$> expp

ifc :: Parser CClause
ifc = do
      keyword "if"
      CCIf <$> expp

clausez :: [CClause] -> Parser [CClause]
clausez cs = do
        for <- forc
        clausez (cs ++ [for])
       <|>
       do
        _if <- ifc
        clausez (cs ++ [_if])
       <|>
        return cs

expz :: Parser [Exp]
expz = do
        n <- expp
        exps n
        <|>
        return []

exps :: Exp -> Parser [Exp]
exps e = do
        symbol ","
        n1 <- expp
        n2 <- exps n1
        return (e:n2)
        <|>
        do
          return [e]


--- Handling of numConst
numConst :: Parser Exp
numConst = do
        Const . IntVal <$> pNum
       <|>
       do
        symbolNoWhiteSpace "-"
        n <- pNumNoWhiteSpace
        return (Const (IntVal (-n)))

pNum :: Parser Int
pNum = lexeme pNumNoWhiteSpace

-- pNumWhiteSpace is used to handle "- 4" which is not allowed.
pNumNoWhiteSpace :: Parser Int
pNumNoWhiteSpace = do
  n <- satisfy (\number -> number /= '0' && isDigit number)
  n1 <- munch1 isDigit
  return $ read (n : n1)
  <|>
  do
  n <- satisfy isDigit
  return $ read [n]

-- Handling of identifiers
reserved :: [String]
reserved = ["None", "True", "False", "for", "if", "in", "not"]

alphaNumOr_ :: Char -> Bool
alphaNumOr_ c = c == '_' || isAlphaNum c

alphaOr_ :: Char -> Bool
alphaOr_ c = c == '_' || isAlpha c

ident :: Parser String
ident = lexeme $ do
  c <- satisfy alphaOr_
  cs <- munch alphaNumOr_
  let word = c:cs
  if word `notElem` reserved then return word
  else pfail

-- Handling of stringConst

-- checks if the strings contains illegal characters and that the characters
-- are printable. illegal chars are ' and \, unless escaped, i.e. \i and \\
stringChecker :: Char -> Bool
stringChecker c = (c /= '\'') && (c /= '\\') && isPrint c

stringSatisfyer :: Parser String
stringSatisfyer = do
  c <- get
  if stringChecker c then do
    return [c]
  else
    if c == '\\' then do
      c1 <- get
      case c1 of
        '\\' -> return [c1]
        '\'' -> return [c1]
        'n' -> return ['\n']
        '\n' -> return ""
        _ -> pfail
    else
      pfail

stringConst :: Parser Exp
stringConst = do
   char '\''
   ds <- many stringSatisfyer
   symbol "'"
   return (Const (StringVal (intercalate "" ds)))

-- Utility functions
-- Added function for checking if something is a comment and then ignoring it.
comments :: Parser ()
comments = do
           many comment
           return ()

comment :: Parser ()
comment = do
           string "#"
           munch (/= '\n')
           char '\n'
           return ()
          <|>
          do
           string "#"
           munch (/= '\n')
           eof

whitespace :: Parser ()
whitespace = do many (satisfy isSpace); return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; comments; return a


symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

keyword :: String -> Parser ()
keyword s = lexeme $ do
  string s
  (c:_) <- look;
  if isAlphaNum c then
    pfail
  else return ()

-- used throughout the functionality where we do not want 'lexeme' to 
-- interact with whitespace, e.g. within strings.
symbolNoWhiteSpace :: String -> Parser ()
symbolNoWhiteSpace s = do string s; return ()





