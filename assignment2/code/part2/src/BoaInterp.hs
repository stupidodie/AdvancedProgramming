-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  ( Env,
    RunError (..),
    Comp (..),
    abort,
    look,
    withBinding,
    output,
    truthy,
    operate,
    apply,
    eval,
    exec,
    execute,
  )
where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String])}

instance Monad Comp where
  return a = Comp (const (Right a, []))
  c >>= f = Comp $ \env -> case runComp c env of
    (Left runError, s) -> (Left runError, s)
    (Right a, s) -> case runComp (f a) env of
      (Left runError, s') -> (Left runError, s `mappend` s')
      (Right a', s') -> (Right a', s `mappend` s')

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM

instance Applicative Comp where
  pure = return
  (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort re = Comp (const (Left re, []))

look :: VName -> Comp Value
look vname = Comp $ \env -> case env of
  [] -> (Left (EBadVar vname), [])
  (x : xs) -> case x of
    (name, value) -> if name == vname then (Right value, []) else runComp (look vname) xs

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding vname value c = Comp $ \env -> runComp c ((vname, value) : env)

output :: String -> Comp ()
output s = Comp (const (Right (), [s]))

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy value = case value of
  NoneVal -> False
  FalseVal -> False
  (IntVal 0) -> False
  (StringVal "") -> False
  (ListVal []) -> False
  _ -> True

operate :: Op -> Value -> Value -> Either String Value
operate op v1 v2 = case op of
  Plus -> case v1 of
    (IntVal a) -> case v2 of
      (IntVal b) -> Right (IntVal (a + b))
      _ -> Left $ "Wrong input Value " ++ show v2
    _ -> Left $ "Wrong input Value " ++ show v1
  Minus -> case v1 of
    (IntVal a) -> case v2 of
      (IntVal b) -> Right (IntVal (a - b))
      _ -> Left $ "Wrong input Value " ++ show v2
    _ -> Left $ "Wrong input Value " ++ show v1
  Times -> case v1 of
    (IntVal a) -> case v2 of
      (IntVal b) -> Right (IntVal (a * b))
      _ -> Left $ "Wrong input Value " ++ show v2
    _ -> Left $ "Wrong input Value " ++ show v1
  Div -> case v1 of
    (IntVal a) -> case v2 of
      (IntVal b) -> if b == 0 then Left "Div 0 error!" else Right (IntVal (a `div` b))
      _ -> Left $ "Wrong input Value " ++ show v2
    _ -> Left $ "Wrong input Value " ++ show v1
  Mod -> case v1 of
    (IntVal a) -> case v2 of
      (IntVal b) -> if b == 0 then Left "Mod 0 error!" else Right (IntVal (a `mod` b))
      _ -> Left $ "Wrong input Value " ++ show v2
    _ -> Left $ "Wrong input Value " ++ show v1
  Eq -> if v1 == v2 then Right TrueVal else Right FalseVal
  Less -> case v1 of
    (IntVal a) -> case v2 of
      (IntVal b) -> if a < b then Right TrueVal else Right FalseVal
      _ -> Left $ "Wrong input Value " ++ show v2
    _ -> Left $ "Wrong input Value " ++ show v1
  Greater -> case v1 of
    (IntVal a) -> case v2 of
      (IntVal b) -> if a > b then Right TrueVal else Right FalseVal
      _ -> Left $ "Wrong input Value " ++ show v2
    _ -> Left $ "Wrong input Value " ++ show v1
  In -> case v2 of
    (ListVal l) -> case l of
      [] -> Right FalseVal
      [x] -> case x of
        (ListVal _) -> operate op v1 x
        _ -> operate Eq x v1
      (x : xs) -> case x of
        (ListVal _) -> do
          v1 <- operate op v1 x
          v2 <- operate op v1 (ListVal xs)
          if v1 == TrueVal || v2 == TrueVal then Right TrueVal else Right FalseVal
        _ -> if v1 `elem` l then Right TrueVal else Right FalseVal
    _ -> Left "The v2 is not a List"

val2str :: Value -> String
val2str v = case v of
  NoneVal -> "None"
  TrueVal -> "True"
  FalseVal -> "False"
  (IntVal x) -> show x
  (StringVal s) -> s
  (ListVal l) -> case l of
    [] -> ""
    [x] -> "[" ++ val2str x ++ "]"
    _ -> "[" ++ concatMap (\x -> if x /= ListVal [] then val2str x ++ ", " else "") (take (length l -1) l) ++ val2str (last l) ++ "]"

vals2str :: [Value] -> String
vals2str l = case l of
  [] -> ""
  [x] -> val2str x
  (x : xs) -> val2str x ++ " " ++ vals2str xs

apply :: FName -> [Value] -> Comp Value
apply "range" l = case length l of
  1 -> case l of
    [IntVal n2] -> apply "range" [IntVal 0, IntVal n2, IntVal 1]
    _ -> abort (EBadArg "Arguments Type not match")
  2 -> case l of
    [IntVal n1, IntVal n2] -> apply "range" [IntVal n1, IntVal n2, IntVal 1]
    _ -> abort (EBadArg "Arguments Type not match")
  3 -> case l of
    [IntVal n1, IntVal n2, IntVal n3] ->
      if n3 == 0
        then abort (EBadArg "n3 is 0")
        else
          if ((n1 >= n2) && (n3 > 0)) || ((n1 <= n2) && (n3 < 0))
            then return (ListVal [])
            else return (ListVal $ map IntVal $ filter (\x -> ((x - n1) `mod` n3 == 0) && x /= n2) [n1 .. n2])
    _ -> abort (EBadArg "Arguments Type not match")
  _ -> abort (EBadArg "The argument numbers is wrong")
-- TODO: The String formatted
-- Have tried the use the function apply, but not work,
--  so use the function val2str and vals2str
apply "print" l = do
  output (vals2str l)
  return NoneVal
apply f _ = abort (EBadFun f)

-- evalOneCC :: Exp -> Comp Value
-- evalOneCC (Compr exp [CCFor v1 e1]) = do
--   e1' <- eval e1
--   case e1' of
--     (ListVal l) -> do
--       s1 <- sequence (fmap (\x -> withBinding v1 x (eval exp)) l)
--       return (ListVal s1)
--     _ -> abort (EBadArg "Return Exp in CCFOr is not a List")
-- evalOneIf::Exp -> Comp Value
-- evalOneIf (Compr exp [CCIf e1]) = do
--   e1' <- eval e1
--   case truthy e1' of
--       True-> -- GO NEXT
--       False->return NoneVal

evalGeneral :: Exp -> Comp Value
-- Take the argument that Compr exp [CClause] and no restriction for the [CClause]
evalGeneral (Compr exp l) = case l of
  [] -> return (ListVal [])
  [x] -> case x of
    (CCFor v1 e1) -> do
      e1' <- eval e1
      case e1' of
        (ListVal l1) -> do
          s1 <- mapM (\x -> withBinding v1 x (eval exp)) l1
          return (ListVal s1)
        _ -> abort (EBadArg "Return Exp in CCFOr is not a List")
    (CCIf e1) -> do
      e1' <- eval e1
      if truthy e1'
        then do
          e' <- eval exp
          return (ListVal [e'])
        else return (ListVal [])
  (x : xs) -> case x of
    (CCFor v1 e1) -> do
      e1' <- eval e1
      case e1' of
        (ListVal l1) -> do
          s1 <- mapM (\x -> withBinding v1 x (evalGeneral (Compr exp xs))) l1
          return (ListVal (concatMap (\(ListVal v2) -> v2) s1))
        _ -> abort (EBadArg "Return Exp in CCFOr is not a List")
    (CCIf e1) -> do
      e1' <- eval e1
      if truthy e1' then evalGeneral (Compr exp xs) else return (ListVal [])
evalGeneral _=abort (EBadArg "Wrong")
-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const v) = return v
eval (Var vname) = look vname
eval (Oper op e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case operate op v1 v2 of
    (Right v) -> return v
    (Left err) -> abort (EBadArg err)
eval (Not e1) = do
  s <- eval e1
  if truthy s then return FalseVal else return TrueVal
eval (Call f exp) = case exp of
  [] -> apply f []
  _ -> do
    s <- mapM eval exp
    apply f s
eval (List exp) = case exp of
  [] -> return (ListVal [])
  (_ : _) -> do
    s <- mapM eval exp
    return (ListVal s)
eval (Compr exp l@((CCFor _ _):_)) = evalGeneral (Compr exp l)
eval (Compr _ _) = abort (EBadArg "Call Comp argument wrong type!")

exec' :: Program -> Comp Value
exec' l = case l of
  [] -> return NoneVal
  [x] -> case x of
    (SDef v e) -> do
      e' <- eval e
      withBinding v e' (exec' [])
    (SExp e) -> do
      eval e
      exec' []
  (x : xs) -> case x of
    (SDef v e) -> do
      e' <- eval e
      withBinding v e' (exec' xs)
    (SExp e) -> do
      eval e
      exec' xs

exec :: Program -> Comp ()
exec l = do
  exec' l
  return ()

execute :: Program -> ([String], Maybe RunError)
execute l = case runComp (exec l) [] of
  (Left e, s) -> (s, Just e)
  (_, s) -> (s, Nothing)