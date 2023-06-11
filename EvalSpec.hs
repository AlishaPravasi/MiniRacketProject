module EvalSpec where


import Test.Hspec
import Parser
import Expr
import MiniRacketParser

import Eval
import Error

type ParseResult = Either ErrorT (Expr, String)

spec :: Spec
spec = do
    describe "evaluating expressions" $ do
        it "evaluates number: 1235" $ 
            evalStr "1235" `shouldBe` Right (IntVal 1235)
        it "evaluates negative numbers: -12235" $
            evalStr "-12235" `shouldBe` Right (IntVal (-12235))
        it "evaluates true" $
            evalStr "true" `shouldBe` Right (BoolVal True)
        it "evaluates false" $
            evalStr "false" `shouldBe` Right (BoolVal False)
    describe "evaluating not" $ do 
        it "evaluates 'not true'" $
            evalStr "(not true)" `shouldBe` Right (BoolVal False)
        it "evaluates 'not false'" $
            evalStr "(not false)" `shouldBe` Right (BoolVal True)
    describe "evaluating and" $ do 
        it "evaluates 'and true false'" $
            evalStr "(and true false)" `shouldBe` Right (BoolVal False)
        it "evaluates 'not false false'" $
            evalStr "(and true true)" `shouldBe` Right (BoolVal True)
    describe "evaluating or" $ do 
        it "evaluates 'and true false'" $
            evalStr "(or true false)" `shouldBe` Right (BoolVal True)
        it "evaluates 'not false false'" $
            evalStr "(or false false)" `shouldBe` Right (BoolVal False)
    describe "evaluating simple expressions" $ do
        it "evaluates addition (+ 1 3)" $
            evalStr "(+ 1 3)" `shouldBe` Right (IntVal 4)
        it "evaluates subtraction (- 4 2)" $
            evalStr "(- 4 2)" `shouldBe` Right (IntVal 2)
        it "evaluates multiply (* 5 5)" $
            evalStr "(* 5 5)" `shouldBe` Right (IntVal 25)
        it "evaluates divide (div 6 3)" $
            evalStr "(div 6 3)" `shouldBe` Right (IntVal 2)
        it "evaluates mod (mod 7 3)" $
            evalStr "(mod 7 3)" `shouldBe` Right (IntVal 1)
        it "evaluates equal? (equal? 6 6)" $
            evalStr "(equal? 6 6)" `shouldBe` Right (BoolVal True)
        it "evaluates equal? (equal? 6 9)" $
            evalStr "(equal? 6 9)" `shouldBe` Right (BoolVal False)
        it "evaluates Lt (< 2 10)" $
            evalStr "(< 10 2)" `shouldBe` Right (BoolVal False)
        it "evaluates Lt (< 10 12)" $
            evalStr "(< 10 12)" `shouldBe` Right (BoolVal True)
    describe "evaluating if statements" $ do
        it "evaluates (if (< 4 5) true false)" $
            evalStr "(if (< 4 5) true false)" `shouldBe` Right (BoolVal True)
        it "evaluates (if (< 5 4) 69 96)" $
            evalStr "(if (< 5 4) 69 96)" `shouldBe` Right (IntVal 96)
    describe "evaluating variables" $ do
        it "evaluates help me" $
            evalStr "help me" `shouldBe` Left (NoSymbol "symbol help not found")
        it "evaluates x" $
            evalStr "x" `shouldBe` Left (NoSymbol "symbol x not found")
    describe "evaluating lets" $ do
        it "evaluates (let (x 5) (+ x 3))" $ do
            evalStr "(let (x 5) (+ x 3))" `shouldBe` Right (IntVal 8)
        it "evaluates (let (x 70) (* x 2))" $ do
            evalStr "(let (x 70) (* x 2))" `shouldBe` Right (IntVal 140)
    describe "evaluating lambdas/apply" $ do
        it "(let (x (lambda (z) (+ z 5))) (x))" $
            evalStr "(let (x (lambda (z) (+ z 5))) (x))" `shouldBe` Right (ClosureVal "x" "z" (MathExpr Add [VarExpr "z",LiteralExpr (IntVal 5)]) [])
        it "(let (x (if (and (< 3 5) (equal? 9 27)) 3 9)) (* x 27) ) (apply is being tested here)" $
            evalStr "(let (x (if (and (< 3 5) (equal? 9 27)) 3 9)) (* x 27) )" `shouldBe` Right (IntVal 243)
    

        
    

        
        