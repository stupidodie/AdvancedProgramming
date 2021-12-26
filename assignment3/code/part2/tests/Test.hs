-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit
import Foreign (Bits(testBit))
import Data.Either (Either(Right))

main :: IO ()
main = defaultMain $ localOption (mkTimeout 2000000) tests

tests = testGroup "Parser Tests: " [
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p
  ,
  testGroup "num Tests"[
  testCase "Positive Number 243543234" $
    parseString "243543234" @?=
      Right [SExp (Const (IntVal 243543234))],
  testCase "Negative Number -32452342" $
    parseString "-32452342" @?=
      Right [SExp (Const (IntVal (-32452342)))],
  testCase "Positive Number 0" $
    parseString "0" @?=
      Right [SExp (Const (IntVal 0))],
  testCase "Positive Number 001" $
    case parseString "001" of
      Left e -> return ()
      Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "Positive Number -0" $
     parseString "-0" @?=
      Right [SExp (Const (IntVal 0))]
  ],
  testGroup "String Tests" [
    testCase "String Test 1" $
      parseString  "'hello_world'" @?=
        Right [SExp (Const (StringVal "hello_world"))],
    testCase "String Test 2" $
      parseString  "'hello_world\n\\\\'" @?=
        Right [SExp (Const (StringVal "hello_world\n\\"))],
    testCase "String Test 3" $
      parseString  "'hello_world\n\\\'\n'" @?=
        Right [SExp (Const (StringVal "hello_world\n'\n"))],
    testCase "String Test 4" $
      parseString  "'#hello\nworld'" @?=
        Right [SExp (Const (StringVal "\nworld"))]
  ],
  testGroup "ident Tests" [
    testCase "None reserved word hello" $
      parseString "hello" @?=
        Right [SExp (Var "hello")],
    testCase "None reserved word hello_world" $
      parseString "hello_world" @?=
        Right [SExp (Var "hello_world")],
    testCase "None reserved word hello_world_123" $
      parseString "hello_world_123" @?=
        Right [SExp (Var "hello_world_123")],
    testCase "None reserved word __value" $
      parseString "__value" @?=
        Right [SExp (Var "__value")],
    testCase "Reserved word if" $
       case parseString "if" of
          Left e -> return ()  
          Right p -> assertFailure $ "Unexpected parse: " ++ show p
          ,
    testCase "Reserved word not" $
        case parseString "not" of
          Left e -> return ()  
          Right p -> assertFailure $ "Unexpected parse: " ++ show p
          ,
    testCase "Reserved word in" $
        case parseString "in" of
          Left e -> return ()  
          Right p -> assertFailure $ "Unexpected parse: " ++ show p
  ],
  testGroup "EXP Tests" [
    testCase "EXP True" $
      parseString "True" @?=
        Right [SExp (Const TrueVal)],
    testCase "EXP None" $
      parseString "None" @?=
        Right [SExp (Const NoneVal)],
    testCase "EXP False" $
      parseString "False" @?=
        Right [SExp (Const FalseVal)],
    testCase "EXP not" $
      parseString "not True" @?=
        Right [SExp (Not (Const TrueVal))],
    testCase "EXP (Expr)" $
      parseString "not (True)" @?=
        Right [SExp (Not (Const TrueVal))],
    testCase "Exp ident ‘(’ Exprz ‘)’" $
      parseString "range (1,5,1)" @?=
        Right [SExp (Call "range" [Const (IntVal 1),Const (IntVal 5),Const (IntVal 1)])],
    testCase "Exp ‘[’ Exprz ‘]’" $
      parseString "[1,2,3]" @?=
        Right [SExp (List [Const (IntVal 1),Const (IntVal 2),Const (IntVal 3)])],
    testCase "Exp ‘[’ Exprz ‘]’" $
      parseString "[]" @?=
        Right [SExp (List [])],
    testCase "Exp ‘[’ Expr ForClause Clausez ‘]’" $
      parseString "[i for i in range(1,5,1)]" @?=
        Right [SExp (Compr (Var "i") [CCFor "i" (Call "range" [Const (IntVal 1),Const (IntVal 5),Const (IntVal 1)])])],
    testCase "Exp ‘[’ Expr ForClause Clausez ‘]’ 2" $
      parseString "[i for i in range(1,5,1) if i > 2]" @?=
        Right [SExp (Compr (Var "i") [CCFor "i" (Call "range" [Const (IntVal 1),Const (IntVal 5),Const (IntVal 1)]),CCIf (Oper Greater (Var "i") (Const (IntVal 2)))])],
    testCase "Exp ‘[’ Expr ForClause Clausez ‘]’ 3" $
      parseString "[i*j for i in range(1,5,1) if i > 2 for j in range(2,9,2)]" @?=
        Right [SExp (Compr (Oper Times (Var "i") (Var "j")) [CCFor "i" (Call "range" [Const (IntVal 1),Const (IntVal 5),Const (IntVal 1)]),CCIf (Oper Greater (Var "i") (Const (IntVal 2))),CCFor "j" (Call "range" [Const (IntVal 2),Const (IntVal 9),Const (IntVal 2)])])],
    testCase "Exp Oper 1" $
      parseString "1 + 2" @?=
        Right [SExp (Oper Plus (Const (IntVal 1)) (Const (IntVal 2)))],
    testCase "Exp Oper 2" $
      parseString "1 - 2" @?=
        Right [SExp (Oper Minus (Const (IntVal 1)) (Const (IntVal 2)))],
    testCase "Exp Oper 3" $
      parseString "1 * 2" @?=
        Right [SExp (Oper Times (Const (IntVal 1)) (Const (IntVal 2)))],
    testCase "Exp Oper 4" $
      parseString "1 // 2" @?=
        Right [SExp (Oper Div (Const (IntVal 1)) (Const (IntVal 2)))],
    testCase "Exp Oper 5" $
      parseString "1 % 2" @?=
        Right [SExp (Oper Mod (Const (IntVal 1)) (Const (IntVal 2)))],
    testCase "Exp Oper 6" $
      parseString "1 == 2" @?=
        Right [SExp (Oper Eq (Const (IntVal 1)) (Const (IntVal 2)))],
    testCase "Exp Oper 7" $
      parseString "1 != 2" @?=
        Right [SExp (Not(Oper Eq (Const (IntVal 1)) (Const (IntVal 2))))],
    testCase "Exp Oper 8" $
      parseString "1 < 2" @?=
        Right [SExp (Oper Less (Const (IntVal 1)) (Const (IntVal 2)))],
    testCase "Exp Oper 9" $
      parseString "1 <= 2" @?=
        Right [SExp (Not (Oper Greater (Const (IntVal 1)) (Const (IntVal 2))))],
    testCase "Exp Oper 10" $
      parseString "1 > 2" @?=
        Right [SExp (Oper Greater (Const (IntVal 1)) (Const (IntVal 2)))],
    testCase "Exp Oper 11" $
      parseString "1 >= 2" @?=
        Right [SExp (Not (Oper Less (Const (IntVal 1)) (Const (IntVal 2))))],
    testCase "Exp Oper 12" $
      parseString " x in x * x" @?=
        Right [SExp (Oper In (Var "x") (Oper Times (Var "x") (Var "x")))],
    testCase "Exp Oper 13" $
      parseString " x not in x*x" @?=
        Right [SExp (Not (Oper In (Var "x") (Oper Times (Var "x") (Var "x"))))],
    testCase "Exp Oper 14" $
      parseString "1*2+3" @?=
        Right [SExp (Oper Plus (Oper Times (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))],
    testCase "Exp Oper 15" $
      parseString "1+2*3" @?=
        Right [SExp (Oper Plus (Const (IntVal 1)) (Oper Times (Const (IntVal 2)) (Const (IntVal 3))))],
    testCase "Exp Oper 16" $
      parseString "1*2+3*4" @?=
        Right [SExp (Oper Plus (Oper Times (Const (IntVal 1)) (Const (IntVal 2))) (Oper Times (Const (IntVal 3)) (Const (IntVal 4))))],
    testCase "Exp Oper 17" $
      parseString "1*(2-3)//4 ==5*2%2" @?=
        Right [SExp (Oper Eq (Oper Div (Oper Times (Const (IntVal 1)) (Oper Minus (Const (IntVal 2)) (Const (IntVal 3)))) (Const (IntVal 4))) (Oper Mod (Oper Times (Const (IntVal 5)) (Const (IntVal 2))) (Const (IntVal 2))))]
  ],
  testGroup "Stmt" [
    testCase "SDef x=4" $
      parseString "x=4" @?=
        Right [SDef "x" (Const (IntVal 4))],
    testCase "SDef x= for i in range(1,5,1)" $
      parseString "x= [i for i in range(1,5,1)]" @?=
        Right [SDef "x" (Compr (Var "i") [CCFor "i" (Call "range" [Const (IntVal 1),Const (IntVal 5),Const (IntVal 1)])])]
  ],
  testGroup "Stmts" [
    testCase "Stmts 1" $
      parseString "x=[i for i in range(1,5,1)];print(x)" @?=
        Right [SDef "x" (Compr (Var "i") [CCFor "i" (Call "range" [Const (IntVal 1),Const (IntVal 5),Const (IntVal 1)])]),SExp (Call "print" [Var "x"])],
    testCase "Stmts 2" $
      parseString "x=[i for i in range (1,5,1)];print (x);j=1;print (j in x)" @?=
        Right [SDef "x" (Compr (Var "i") [CCFor "i" (Call "range" [Const (IntVal 1),Const (IntVal 5),Const (IntVal 1)])]),SExp (Call "print" [Var "x"]),SDef "j" (Const (IntVal 1)),SExp (Call "print" [Oper In (Var "j") (Var "x")])]
  ],
  testGroup "Comment"[
    testCase "Comment 1" $
      parseString "x=[i for i in range(1,5,1)];#comment\nprint(x)" @?=
        Right [SDef "x" (Compr (Var "i") [CCFor "i" (Call "range" [Const (IntVal 1),Const (IntVal 5),Const (IntVal 1)])]),SExp (Call "print" [Var "x"])],
    testCase "Comment 2" $
      parseString "2+#Comments\n3" @?=
        Right [SExp (Oper Plus (Const (IntVal 2)) (Const (IntVal 3)))]
  ]
      ]
