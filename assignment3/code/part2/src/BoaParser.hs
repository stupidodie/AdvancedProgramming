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
import Debug.Trace
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
    manyTill,
    anyChar,
    eof,
    lookAhead,
    char,
    parseTest 
  )

-- add any other other imports you need

-- Due to the library Text.ParserCombinators.Parsec
--  already defined the ParseError, so there is no
--  need to redefine this
-- type ParseError = String -- you may replace this

parseString :: String -> Either ParseError Program
parseString = runParser (do e<-program; eof;return e) () ""

program :: Parser Program
program =   lexeme stmts

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
stmt =lexeme $
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
-- None factoring: Op4: in, not in,
-- Thus we rewrite the grammar in the Expr Oper Expr
-- Exp  = Exp1 Op4 Exp1 | Exp1
-- Exp1 = Exp2 Op3 Exp2 | Exp2
-- Exp2 = Exp4 Exp3
-- Exp3 = Op2 Exp4 Exp3| eps
-- Exp4 = Exp6 Exp5
-- Exp5 = Op1 Exp6 Exp5| eps
-- Exp6 = Value
expParse :: Parser Exp
expParse = lexeme $ try
  (do
      e1<- exp1
      symbol "in"
      Oper In e1 <$> exp1
  )
  <|> try
    (
      do
        e1<- exp1
        symbol "not"
        symbol "in"
        Not . Oper In e1 <$> exp1
    )
    <|>exp1

exp1 :: Parser Exp
exp1 =lexeme $
  try
    ( do
        e1 <- exp2
        symbol "=="
        Oper Eq e1 <$> exp2
    )
    <|> try
      ( do
          e1 <- exp2
          symbol "!="
          Not . Oper Eq e1 <$> exp2
      )
    <|> try
      ( do
          e1 <- exp2
          symbol "<"
          Oper Less e1 <$> exp2
      )
    <|> try
      ( do
          e1 <- exp2
          symbol ">"
          Oper Greater e1 <$> exp2
      )
    <|> try
      ( do
          e1 <- exp2
          symbol "<="
          Not . Oper Greater e1 <$> exp2
      )
    <|> try
      ( do
          e1 <- exp2
          symbol ">="
          Not . Oper Less e1 <$> exp2
      )
    <|> try exp2
exp2 :: Parser Exp
exp2 = lexeme $ do
  e1 <- exp4
  spaces
  e2 <- exp3
  case e2 of
    Nothing -> return e1
    Just e -> return (e e1)

exp3 :: Parser (Maybe (Exp -> Exp))
exp3 =lexeme $
  do
    symbol "+"
    e1 <- exp4
    spaces
    e2 <- exp3
    case e2 of
      Nothing -> return (Just (\e -> Oper Plus e e1))
      Just e -> return (Just (\e' -> e (Oper Plus e' e1)))
  <|> do
      symbol "-"
      e1 <- exp4
      spaces
      e2 <- exp3
      case e2 of
        Nothing -> return (Just (\e -> Oper Minus e e1))
        Just e -> return (Just (\e' -> e (Oper Minus e' e1)))
  <|> return Nothing
exp4 :: Parser Exp
exp4 = lexeme $ do
  e1 <- exp6
  spaces
  e2 <- exp5
  case e2 of
    Nothing -> return e1
    Just e -> return (e e1)
exp5 :: Parser (Maybe (Exp -> Exp))
exp5 =lexeme $
  do
    symbol "%"
    e1 <- exp6
    spaces
    e2 <- exp5
    case e2 of
      Nothing -> return (Just (\e -> Oper Mod e e1))
      Just e -> return (Just (\e' -> e (Oper Mod e' e1)))
    <|> do
      symbol "//"
      e1 <- exp6
      spaces
      e2 <- exp5
      case e2 of
        Nothing -> return (Just (\e -> Oper Div e e1))
        Just e -> return (Just (\e' -> e (Oper Div e' e1)))
    <|> do
      symbol "*"
      e1 <- exp6
      spaces
      e2 <- exp5
      case e2 of
        Nothing -> return (Just (\e -> Oper Times e e1))
        Just e -> return (Just (\e' -> e (Oper Times e' e1)))
    <|> return Nothing

exp6 :: Parser Exp
exp6 = lexeme exprParser

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
    <|> try (do
      symbol "("
      e <- expParse
      symbol ")"
      return e
    )
    <|> try (
      Var <$> ident
    )
    <|> try
      ( do
          symbol "["
          e <- exprz
          symbol "]"
          return (List e)
      )
    <|> try (
      do
      symbol "["
      exp <- expParse
      forcc <- forClause
      l <- clausez
      symbol "]"
      return (Compr exp (forcc : l))
    )
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
symbol s = lexeme $ do
  string s
  return ()

ident :: Parser String
ident = lexeme $ do
  first <- satisfy (\x -> x == '_' || isLetter x)
  rest <- many $satisfy (\x -> x == '_' || isLetter x || isNumber x)
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
        if numFirst == '0'
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
stringCheck:: Parser String
stringCheck = try (
    do
      a <- satisfy (\x->isPrint x||x=='\\'||x=='\n')
      -- traceM $show a
      if a == '\''
        then unexpected "Meet the end"
        else if a == '\\'
            then do
              b<- satisfy (\x->isPrint x||x=='\\'||x=='\n')
              -- traceM $ show b
              case b of
                'n'-> return "\n"
                '\''-> return "'"
                '\n'-> return ""
                '\\'-> return "\\"
                _->unexpected $ "After \\ is an unacceptable char" ++ show b
            else return [a] )

stringConst :: Parser Exp
stringConst = do
  char '\''
  s <- many stringCheck
  string "'"
  return (Const (StringVal (concat s)))
comment::Parser ()
comment=try (
  do
    string "#"
    manyTill anyChar (try (string "\n"))
    return ()
  )<|> return ()
lexeme :: Parser a -> Parser a
lexeme x = do try spaces; a <- x; try spaces; comment; return a
main = 
  parseTest stringConst  "'fo\\\\o\nb\na\\\\\'r'"