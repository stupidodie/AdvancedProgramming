module ExprEval where

import ExprAst
import qualified Data.Map.Strict as M
import Data.Map(Map)
import Debug.Trace
import Data.List(isInfixOf)
import GHC.Base (Bool(False))
type Env = Map String Int

oper :: Op -> (Int -> Int -> Int)
oper Plus = (+)
oper Minus = (-)
oper Times = (*)

eval :: Expr -> Env -> Either String Int
eval (Const n) env = return n
eval (Oper op x y) env = oper op <$> eval x env <*> eval y env
eval (Var v) env = case M.lookup v env of
                     Nothing -> Left ("Unknown identifier: "++v)
                     Just val -> return val
eval (Let v e body) env = do
  val <- eval e env
  eval body $ M.insert v val env

evalTop e = eval e M.empty


findVar:: Ident ->Expr->Bool 
findVar v (Oper _ e1 e2)= findVar v e1 || findVar v e2
findVar v (Var v')=  v==v' 
findVar v (Const _)=False
findVar v (Let v' e1 e2)=  v==v' || findVar v e1|| findVar v e2 





simplify e =
  case e of
    Oper Plus (Const c1) (Const c2) -> Const(c1+c2)
    Oper Minus (Const c1) (Const c2) -> Const(c1-c2)
    Oper Times (Const 1) (Const c2) -> Const c2
    Oper Times (Const c1) (Const 1) -> Const c1
    Oper Times (Const 0) (Const c2) -> Const 0
    Oper Times (Const c1) (Const 0) -> Const 0
    Oper Times (Const c1) (Const c2) -> Const(c1*c2)
    Oper op e1 e2 -> Oper op (simplify e1) (simplify e2)
    Let v e body ->if findVar v e
        then Let v (simplify e) (simplify  body)
        else simplify body
    _ -> e
