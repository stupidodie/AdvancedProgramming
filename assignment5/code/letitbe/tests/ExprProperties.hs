module ExprProperties where
import Test.QuickCheck

import ExprAst
import qualified ExprEval as E
import Data.List(isInfixOf)

instance Arbitrary Expr where
   arbitrary = expr

expr::Gen Expr
expr = sized exprN
exprN 0=fmap Const arbitrary
exprN n= oneof [fmap Const arbitrary 
         ,fmap Var  arbitrary
         ,Oper Plus <$> subexpr <*> subexpr 
         , Oper Minus <$> subexpr <*> subexpr
         , Oper Times <$> subexpr <*> subexpr
         -- Need to assume that the first exp contains no Var expression
         , Let <$> listOf arbitrary <*> (subexpr `suchThat` (\x->not $ "Var" `isInfixOf` show x)) <*>subexpr
         ] where subexpr = exprN (n `div` 2 )

prop_eval_simplify :: Expr -> Property
prop_eval_simplify x = E.eval ( E.simplify  x) mempty  === E.eval x mempty
