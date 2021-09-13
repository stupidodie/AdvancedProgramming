-- This is a skeleton file for you to edit
module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

showExp :: Exp -> String
showExp (Cst e) = if e <0  then "(" ++ show e ++ ")" else show e
showExp (Add e1 e2) = "(" ++ showExp e1 ++ "+" ++ showExp e2 ++ ")"
showExp (Sub e1 e2) = "(" ++ showExp e1 ++ "-" ++ showExp e2 ++ ")"
showExp (Mul e1 e2) = "(" ++ showExp e1 ++ "*" ++ showExp e2 ++ ")"
showExp (Div e1 e2) = "(" ++ showExp e1 ++ "`div`" ++ showExp e2 ++ ")"
showExp (Pow e1 e2) = "(" ++ showExp e1 ++ "^" ++ showExp e2 ++ ")"
showExp e = error $ "Operator Error! Which is"++ show e 

evalSimple :: Exp -> Integer
evalSimple (Cst e) = e
evalSimple (Add e1 e2) = evalSimple e1 + evalSimple e2 
evalSimple (Sub e1 e2) = evalSimple e1 - evalSimple e2  
evalSimple (Mul e1 e2) = evalSimple e1 * evalSimple e2
evalSimple (Pow e1 e2) 
  | evalSimple e2 <0 = error "The exponent should be a Non-negative number" 
  | (evalSimple e1 /=0) && (evalSimple e2 ==0)  = 1
  | (evalSimple e1 ==0 ) && (evalSimple e2 ==0) = 1  
  | (evalSimple e1 ==0 ) && (evalSimple e2 /=0) = 0 
  | (evalSimple e1 /=0) && (evalSimple e2 /=0) = evalSimple e1 ^ evalSimple e2
  | otherwise =error "Unknown Error!"
evalSimple (Div e1 e2) 
  | evalSimple e2  == 0  = error "Error! A division by zero!"
  | evalSimple e1 /=0 =evalSimple e1 `div` evalSimple e2 
  | evalSimple e1 ==0 =evalSimple e1 `div` evalSimple e2 
  | otherwise =error "Unknown Error!"
evalSimple _ =error "Can't match the type!"

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r = \x -> if v==x then Just n else r x


evalFull :: Exp -> Env -> Integer
evalFull (If test yes no) env= if evalFull test env==0 then evalFull no env else evalFull yes env
evalFull (Var vname) env=
  case env vname  of 
    Nothing-> error ("no current value for " ++ vname )
    Just value -> value
evalFull (Let var def body ) env = evalFull body (extendEnv var (evalFull def env) env)
evalFull (Sum var from to body) env 
  | evalFull from env > evalFull to env = 0
  | otherwise = evalFull (Let var from body) env + evalFull (Sum var (Add from (Cst 1)) to body ) env
evalFull (Cst e) _ = e 
evalFull (Add e1 e2) env=  evalFull e1 env + evalFull e2 env
evalFull (Sub e1 e2) env=  evalFull e1 env - evalFull e2 env
evalFull (Mul e1 e2) env=  evalFull e1 env *  evalFull e2 env
evalFull (Pow e1 e2) env
  |evalFull e2 env <0 = error "The exponent should be a Non-negative number"
  |(evalFull e2 env == 0) && (evalFull e1 env ==0) = 1
  |(evalFull e2 env /= 0) && (evalFull e1 env ==0) = 0
  |(evalFull e2 env == 0) && (evalFull e1 env /=0) = 1
  |(evalFull e2 env /= 0) && (evalFull e1 env /=0) = evalFull e1 env ^  evalFull e2 env
  |otherwise = error "Unknown Error!"
evalFull (Div e1 e2) env 
  |evalFull e2 env == 0  = error "Error! A division by zero!"
  |(evalFull e2 env /= 0) &&(evalFull e1 env ==0) = 0
  |(evalFull e2 env /= 0) &&(evalFull e1 env /=0) =evalFull e1 env `div` evalFull e2 env 
  |otherwise = error "Unknown Error!"

-- fmap' is a Higher Order function which takes a function 
-- as argument and to apply on the Either Type when it 
-- has the Right value on both two arguments
fmap'::(Ord a)=>(Integer->Integer->a)->Either ArithError Integer -> Either ArithError Integer ->Either ArithError a
fmap' f (Right r1) (Right r2) =Right (f r1 r2)
fmap' _ (Left r1) _ = Left r1
fmap' _  _ (Left r2)  =Left r2


evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (If test yes no) env =  
  case fmap' (==) (evalErr test env) (Right 0) of 
    (Right True) -> evalErr no env 
    (Right False)-> evalErr yes env
    (Left errorMessage)-> Left errorMessage
evalErr (Var vname) env=
  case env vname  of 
    Nothing-> Left (EBadVar vname)
    Just value -> Right value
evalErr (Let var def body ) env = 
  case evalErr def env of
    (Right value) ->  evalErr body (extendEnv var value env)
    (Left errorMessage)-> Left errorMessage
evalErr (Sum var from to body) env = 
  case fmap' (>) (evalErr from env ) (evalErr to env) of
    (Right True) ->  Right 0
    (Right False)->  fmap' (+) (evalErr (Let var from body) env)  (evalErr (Sum var (Add from (Cst 1)) to body ) env)
    (Left errorMessage)-> Left errorMessage
evalErr (Cst e) _ = Right e 
evalErr (Add e1 e2) env =  fmap' (+) (evalErr e1 env)  (evalErr e2 env)
evalErr (Sub e1 e2) env =  fmap' (-) (evalErr e1 env)  (evalErr e2 env)
evalErr (Mul e1 e2) env =  fmap' (*) (evalErr e1 env )  (evalErr e2 env)
evalErr (Pow e1 e2) env = --For the Pow part, first check the if e1 has error, if not then check the Left ENegPower error 
  case evalErr e1 env of
    (Left errorMessage)->Left errorMessage
    (Right _) ->
      case evalErr e2 env of
        (Right n) -> if n<0 then Left ENegPower else  if n==0 then Right 1 else fmap' (^) (evalErr e1 env)  (evalErr e2 env)
        (Left errorMessage) -> Left errorMessage
evalErr (Div e1 e2) env = --For the Div part, first check the if e1 has error, if not then check the Left EDivZero error 
  case evalErr e1 env of 
    (Left errorMessage)-> Left errorMessage
    (Right _)->
      case evalErr e2 env of 
      (Right 0) -> Left EDivZero
      (Right _)-> fmap' div  (evalErr e1 env) ( evalErr e2 env )
      (Left errorMessage)-> Left errorMessage

-- optional parts (if not attempted, leave them unmodified)
showPriority:: Exp  -> Int
showPriority (Cst n) = if n <0 then 5 else 0
showPriority (Add _ _)=1
showPriority (Sub _ _)=1
showPriority (Mul _ _)=2
showPriority (Div _ _)=3
showPriority (Pow _ _)=4
showPriority errorMessage = error $show  errorMessage

showCompact :: Exp -> String
showCompact (Cst e) = show e
showCompact (Add e1 e2) = if showPriority e2 >= 1 
  then showCompact e1 ++ "+" ++ "(" ++ showCompact e2++")" 
  else showCompact e1 ++ "+" ++ showCompact e2
showCompact (Sub e1 e2) = if showPriority e2 >= 1 
  then showCompact e1 ++ "-"++ "(" ++ showCompact e2 ++ ")" 
  else showCompact e1 ++ "-" ++showCompact e2
showCompact (Mul e1 e2)  
  | showPriority e2 >= 2  && (showPriority e1 >=2 && showPriority e1 /= 5|| showPriority e1 ==0 ) = showCompact e1 ++ "*" ++ "(" ++ showCompact e2 ++")" 
  | showPriority e2 >=2 && ((showPriority e1 <2   && showPriority e1 /=0)|| showPriority e1 ==5) = "(" ++ showCompact e1 ++ ")" ++ "*" ++ "(" ++showCompact e2 ++")"
  | showPriority e2 <2  && ((showPriority e1 <2   && showPriority e1 /=0)||showPriority e1 ==5)  = "(" ++ showCompact e1 ++ ")" ++ "*" ++ showCompact e2 
  | otherwise =showCompact e1  ++ "*" ++showCompact e2 
showCompact (Div e1 e2)  
  | showPriority e2 >= 3 && ( showPriority e1 >=2 && showPriority e1 /= 5 || showPriority e1 ==0)= showCompact e1 ++ "`div`" ++ "(" ++ showCompact e2 ++")" 
  | showPriority e2 >=3 && ( (showPriority e1 <2   && showPriority e1/=0) || showPriority e1 ==5)   = "(" ++ showCompact e1 ++ ")" ++ "`div`" ++ "(" ++showCompact e2 ++")"
  | showPriority e2 <3 && ((showPriority e1 <2   && showPriority e1/=0) || showPriority e1 ==5)  = "(" ++ showCompact e1 ++ ")" ++ "`div`" ++ showCompact e2 
  | otherwise =showCompact e1  ++ "`div`" ++showCompact e2 
showCompact (Pow e1 e2) 
  | showPriority e2 > 4 && (showPriority e1 >4 && showPriority e1 /= 5 || showPriority e1 ==0) = showCompact e1 ++ "^" ++ "(" ++ showCompact e2 ++")" 
  | showPriority e2 <4 && (showPriority e1 <4 && showPriority e1/=0) || showPriority e1 ==5  = "(" ++ showCompact e1 ++ ")" ++ "^" ++ "(" ++showCompact e2 ++")"
  | showPriority e1 == 4  = "(" ++ showCompact e1 ++ ")" ++ "^"  ++showCompact e2 
  | otherwise = showCompact e1  ++ "^"  ++showCompact e2 
showCompact errorMessage =error $ show errorMessage

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = evalErr

-- The function extendEnv' function unlike the extendEnv function,
--  it takes the Either ArithError Integer type as an argument rather than 
--  Integer. So it is need to match to get the Integer inside
extendEnv' :: VName -> Either ArithError Integer -> Env -> Env
extendEnv' v (Right n) r = \x -> if v==x then Just n else r x
extendEnv' _ _ r = r

-- The difference between the function evalLazy and 
-- the evalErr is the way it's handle the "Let" part  
evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy (If test yes no) env =  
  case fmap' (==) (evalLazy test env) (Right 0) of 
    (Right True) -> evalLazy no env 
    (Right False)-> evalLazy yes env
    (Left errorMessage)-> Left errorMessage
evalLazy (Var vname) env=
  case env vname  of 
    Nothing-> Left (EBadVar vname)
    Just value -> Right value
-- For the "Let" part handle, the evalLazy function 
-- just use the lazy trait to calculate the result 
evalLazy (Let var def body ) env = evalLazy body (extendEnv' var (evalLazy def env) env) 
evalLazy (Sum var from to body) env = 
  case fmap' (>) (evalLazy from env ) (evalLazy to env) of
    (Right True) ->  Right 0
    (Right False)->  fmap' (+) (evalLazy (Let var from body) env)  (evalLazy (Sum var (Add from (Cst 1)) to body ) env)
    (Left errorMessage)-> Left errorMessage
evalLazy (Cst e) _ = Right e 
evalLazy (Add e1 e2) env= fmap' (+) (evalLazy e1 env)  (evalLazy e2 env)
evalLazy (Sub e1 e2) env=  fmap' (-) (evalLazy e1 env)  (evalLazy e2 env)
evalLazy (Mul e1 e2) env=  fmap' (*) (evalLazy e1 env )  (evalLazy e2 env)
evalLazy (Pow e1 e2) env =
  case evalLazy e1 env of
    (Left errorMessage)->Left errorMessage
    (Right _) ->
      case evalLazy e2 env of
        (Right n) -> if n<0 then Left ENegPower else  if n==0 then Right 1 else fmap' (^) (evalLazy e1 env)  (evalLazy e2 env)
        (Left errorMessage) -> Left errorMessage 
evalLazy (Div e1 e2) env =
  case evalLazy e1 env of 
    (Left errorMessage)-> Left errorMessage
    (Right _)->
      case evalLazy e2 env of 
      (Right 0) -> Left EDivZero
      (Right _)-> fmap' div  (evalLazy e1 env) ( evalLazy e2 env )
      (Left errorMessage)-> Left errorMessage                              