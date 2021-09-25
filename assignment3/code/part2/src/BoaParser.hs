-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
  ( CClause (..),
    Exp (..),
    Op (Div, Eq, Greater, In, Less, Minus, Mod, Plus, Times),
    Program,
    Stmt (..),
    Value (FalseVal, IntVal, NoneVal, StringVal, TrueVal),
  )
import Data.Char (isLetter, isNumber, isPrint)
import Text.ParserCombinators.Parsec
  ( ParseError,
    Parser,
    digit,
    many,
    runParser,
    satisfy,
    spaces,
    string,
    try,
    unexpected,
    (<|>),
  )

-- add any other other imports you need

-- Due to the library Text.ParserCombinators.Parsec
--  already defined the ParseError, so there is no
--  need to redefine this
-- type ParseError = String -- you may replace this

parseString :: String -> Either ParseError Program
parseString = runParser program () ""

program :: Parser Program
program = stmts

stmts :: Parser [Stmt]
stmts =
  try
    ( do
        s <- stmt
        symbol ";"
        l <- stmts
        return (s : l)
    )
    <|> do
      s <- stmt
      return [s]

stmt :: Parser Stmt
stmt =
  try
    ( do
        vname <- ident
        symbol "="
        SDef vname <$> expParse
    )
    <|> do
      SExp <$> expParse

-- We list the following 4 types of different priority of operators
-- Priority:Op1>Op2>Op3>Op4
-- Left factoring: Op1: %, *, //
-- Left factoring: Op2: +, -
-- None factoring: Op3: ==, !=, <, <=, >, >=
-- Left factoring: Op4: in, not in,
-- Thus we change the grammar in the Expr Oper Expr
-- Exp=E1 E2
-- E2= Op4 E1 E2|eps
-- E1=E3Op3E3|E3
-- E3=E4E5
-- E5=Op2E4E5|eps
-- E4=E6E7
-- E7=Op1E6E7|eps
-- E6=Others Exp
expParse :: Parser Exp
expParse = do
  e1 <- exp1
  e2 <- exp2
  case e2 of
    Nothing -> return e1
    Just e -> return (e e1)

exp2 :: Parser (Maybe (Exp -> Exp))
exp2 =
  do
    symbol "in"
    e1 <- exp1
    e2 <- exp2
    case e2 of
      Nothing -> return (Just (\e -> Oper In e e1))
      Just e -> return (Just (\e' -> e (Oper In e' e1)))
    <|> do
      symbol "not"
      symbol "in"
      e1 <- exp1
      e2 <- exp2
      case e2 of
        Nothing -> return (Just (\e -> Not (Oper In e e1)))
        Just e -> return (Just (\e' -> e (Not (Oper In e' e1))))
    <|> return Nothing

exp1 :: Parser Exp
exp1 =
  try
    ( do
        e1 <- exp3
        symbol "=="
        Oper Eq e1 <$> exp3
    )
    <|> try
      ( do
          e1 <- exp3
          symbol "!="
          Not . Oper Eq e1 <$> exp3
      )
    <|> try
      ( do
          e1 <- exp3
          symbol "<"
          Oper Less e1 <$> exp3
      )
    <|> try
      ( do
          e1 <- exp3
          symbol ">"
          Oper Greater e1 <$> exp3
      )
    <|> try
      ( do
          e1 <- exp3
          symbol "<="
          Not . Oper Greater e1 <$> exp3
      )
    <|> try
      ( do
          e1 <- exp3
          symbol ">="
          Not . Oper Less e1 <$> exp3
      )
    <|> exp3

exp3 :: Parser Exp
exp3 = do
  e1 <- exp4
  e2 <- exp5
  case e2 of
    Nothing -> return e1
    Just e -> return (e e1)

exp5 :: Parser (Maybe (Exp -> Exp))
exp5 =
  do
    symbol "+"
    e1 <- exp4
    e2 <- exp5
    case e2 of
      Nothing -> return (Just (\e -> Oper Plus e e1))
      Just e -> return (Just (\e' -> e (Oper Plus e' e1)))
    <|> do
      symbol "-"
      e1 <- exp4
      e2 <- exp5
      case e2 of
        Nothing -> return (Just (\e -> Oper Minus e e1))
        Just e -> return (Just (\e' -> e (Oper Minus e' e1)))
    <|> return Nothing

exp7 :: Parser (Maybe (Exp -> Exp))
exp7 =
  do
    symbol "%"
    e1 <- exp6
    e2 <- exp7
    case e2 of
      Nothing -> return (Just (\e -> Oper Mod e e1))
      Just e -> return (Just (\e' -> e (Oper Mod e' e1)))
    <|> do
      symbol "//"
      e1 <- exp6
      e2 <- exp7
      case e2 of
        Nothing -> return (Just (\e -> Oper Div e e1))
        Just e -> return (Just (\e' -> e (Oper Div e' e1)))
    <|> do
      symbol "*"
      e1 <- exp6
      e2 <- exp7
      case e2 of
        Nothing -> return (Just (\e -> Oper Times e e1))
        Just e -> return (Just (\e' -> e (Oper Times e' e1)))
    <|> return Nothing

exp4 :: Parser Exp
exp4 = do
  e1 <- exp6
  e2 <- exp7
  case e2 of
    Nothing -> return e1
    Just e -> return (e e1)

exp6 :: Parser Exp
exp6 = exprParser

exprParser :: Parser Exp
exprParser =
  numConst
    <|> do
      symbol "None"
      return (Const NoneVal)
    <|> do
      symbol "True"
      return (Const TrueVal)
    <|> do
      symbol "False"
      return (Const FalseVal)
    -- First, try whether the call can be match
    <|> try
      ( do
          s <- ident
          symbol "("
          l <- exprz
          symbol ")"
          return (Call s l)
      )
    <|> do
      symbol "not"
      Not <$> expParse
    <|> do
      symbol "("
      e <- expParse
      symbol "("
      return e
    <|> do
      Var <$> ident
    <|> try
      ( do
          symbol "["
          e <- exprz
          symbol "]"
          return (List e)
      )
    <|> do
      symbol "["
      exp <- expParse
      forcc <- forClause
      l <- clausez
      symbol "]"
      return (Compr exp (forcc : l))
    <|> stringConst

ifClause :: Parser CClause
ifClause = do
  symbol "if"
  CCIf <$> expParse

forClause :: Parser CClause
forClause = do
  symbol "for"
  vname <- ident
  symbol "in"
  CCFor vname <$> expParse

clausez :: Parser [CClause]
clausez =
  do
    forcc <- forClause
    listcc <- clausez
    return (forcc : listcc)
    <|> do
      ifcc <- ifClause
      listcc <- clausez
      return (ifcc : listcc)
    <|> return []

exprz :: Parser [Exp]
exprz =
  exprs
    <|> return []

exprs :: Parser [Exp]
exprs =
  try
    ( do
        exp <- expParse
        symbol ","
        l <- exprs
        return (exp : l)
    )
    <|> do
      exp <- expParse
      return [exp]

symbol :: String -> Parser ()
symbol s = do
  string s
  spaces
  return ()

ident :: Parser String
ident = do
  first <- satisfy (\x -> x == '_' || isLetter x)
  rest <- many $satisfy (\x -> x == '_' || isLetter x || isNumber x)
  spaces
  ( \s ->
      if s `elem` ["None", "True", "False", "for", "if", "in", "not"]
        then unexpected $ "ident Name Crash: " ++ s
        else return s
    )
    (first : rest)

numConst :: Parser Exp
numConst =
  try
    ( do
        satisfy (== '-')
        numFirst <- digit
        numRest <- many digit
        if numFirst == '0' && numRest /= []
          then unexpected $ "illegal Number " ++ (numFirst : numRest)
          else return (Const (IntVal (-1 * (read :: String -> Int) (numFirst : numRest))))
    )
    <|> do
      numFirst <- digit
      numRest <- many digit
      if numFirst == '0' && numRest /= []
        then unexpected $ "illegal Number " ++ (numFirst : numRest)
        else return (Const (IntVal ((read :: String -> Int) (numFirst : numRest))))

-- TODO:Need to handle the String
stringConst :: Parser Exp
stringConst = do
  string "'"
  s <- many $satisfy (\x -> isPrint x || (x == '\n') || (x /= '\''))
  string "'"
  return (Const (StringVal s))

main = print (parseString "squares = [x*x for x in range(10)];print([123, [squares, print(321)]]);print('Odd squares:', [x for x in squares if x % 2 == 1]);n = 5;composites = [j for i in range(2, n) for j in range(i*2, n*n, i)];print('Printing all primes below', n*n);[print(x) for x in range(2,n*n) if x not in composites]")