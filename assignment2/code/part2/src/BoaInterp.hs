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
      (Left runError, s') -> (Left runError, s')
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
    (ListVal l) -> if v1 `elem` l then Right TrueVal else Right FalseVal
    _ -> Left "The v2 is not a List"

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
apply "print" l = case l of
  [] -> return NoneVal
  [x] -> case x of
    NoneVal -> do
      output "None"
      apply "print" []
    TrueVal -> do
      output "True"
      apply "print" []
    FalseVal -> do
      output "True"
      apply "print" []
    (IntVal x) -> do
      output $ show x
      apply "print" []
    (StringVal s) -> do
      output s
      apply "print" []
    (ListVal l) -> case l of
      [] -> do
        output "[]"
        apply "print" []
      [_] -> do
        output "["
        apply "print" l
        output "]"
        apply "print" []
      (_ : _) -> do
        output "["
        head $ fmap (\x -> apply "print" [x] >>= (\_ -> apply "print" [StringVal ", "])) (take (length l -1) l)
        apply "print" [last l]
        output "]"
        apply "print" []
  (x : xs) -> case x of
    NoneVal -> do
      output "None "
      apply "print" xs
    TrueVal -> do
      output "True "
      apply "print" xs
    FalseVal -> do
      output "False "
      apply "print" xs
    (IntVal x) -> do
      output (show x ++ " ")
      apply "print" xs
    (StringVal s) -> do
      output (s ++ " ")
      apply "print" xs
    (ListVal l) -> case l of
      [] -> do
        output "[] "
        apply "print" xs
      [_] -> do
        output "["
        apply "print" l
        output "] "
        apply "print" xs
      (_ : _) -> do
        output "["
        head $ fmap (\x -> apply "print" [x] >>= (\_ -> apply "print" [StringVal ", "])) (take (length l -1) l)
        apply "print" [last l]
        output "] "
        apply "print" xs
apply f _ = abort (EBadFun f)

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const v) = return v
eval (Var vname) = look vname
eval (Oper op e1 e2) = do
  return (operate op)
  eval e1
  eval e2
eval (Not e1) = do
  s <- eval e1
  return (if truthy s == True then FalseVal else TrueVal)
eval (Call f exp) = case exp of
  [] -> apply f []
  (x : xs) -> do
    x1<-eval x
    eval (Call f xs)
  
exec :: Program -> Comp ()
exec = undefined

execute :: Program -> ([String], Maybe RunError)
execute = undefined