module MiniRacketParserSpec where 

import Test.Hspec
import Parser
import Expr 
import MiniRacketParser
import Error

type ParseResult = Either ErrorT (Expr, String)

expr :: Either ErrorT (a2, b) -> a2
expr (Right (e, _)) = e 
expr (Left (SyntaxError msg)) = error msg
expr (Left (ParseError msg)) = error msg
expr (Left NoParse) = error "no matching parse"
expr _ = error "expr in MiniRacketParser.hs is not fully implemented yet..."

spec :: Spec 
spec = do 
    describe "parse literals" $ do
        it "parses number: 1235" $ 
            parseStr "1235" `shouldBe` Right (LiteralExpr (IntVal 1235),"")
        it "parses negative numbers: -12235" $
            parseStr "-12235" `shouldBe` Right (LiteralExpr (IntVal (-12235)), "")
        it "parses true" $
            parseStr "true" `shouldBe` Right (LiteralExpr (BoolVal True), "")
        it "parses false" $
            parseStr "false" `shouldBe` Right (LiteralExpr (BoolVal False), "")
    describe "parse BoolOp" $ do
        it "parses BoolOp: and" $
            parse parseBoolOp "and" `shouldBe` Right ((And), "")
        it "parses BoolOp: or" $
            parse parseBoolOp "or" `shouldBe` Right ((Or), "")
    describe "parse CompOp" $ do
        it "parses CompOp: equal?" $
            parse parseCompOp "equal?" `shouldBe` Right ((Eq), "")
        it "parses CompOp: <" $
            parse parseCompOp "<" `shouldBe` Right ((Lt), "")
    describe "parse MathOp" $ do
        it "parses MathOp: +" $
            parse parseMathOp "+" `shouldBe` Right ((Add), "")
        it "parses MathOp: -" $
            parse parseMathOp "-" `shouldBe` Right ((Sub), "")
        it "parses MathOp: *" $
            parse parseMathOp "*" `shouldBe` Right ((Mul), "")
        it "parses MathOp: div" $
            parse parseMathOp "div" `shouldBe` Right ((Div), "")
        it "parses MathOp: mod" $
            parse parseMathOp "mod" `shouldBe` Right ((Mod), "")
    describe "parse notExpr" $ do
        it "parses notExpr: not true" $
            parseStr "(not true)" `shouldBe` Right (NotExpr (LiteralExpr (BoolVal True)), "")
        it "parses notExpr: not false" $
            parseStr "(not false)" `shouldBe` Right (NotExpr (LiteralExpr (BoolVal False)), "")
    describe "parse boolExpr 'and' and 'or'" $ do
        it "parses boolExpr: (or true false)" $
            parseStr "(or true false)" `shouldBe` Right (BoolExpr Or [LiteralExpr (BoolVal True), LiteralExpr (BoolVal False)], "")
        it "parses boolExpr: (or false false)" $
            parseStr "(or false false)" `shouldBe` Right (BoolExpr Or [LiteralExpr (BoolVal False), LiteralExpr (BoolVal False)], "")
        it "parses boolExpr: (and true false)" $
            parseStr "(and true false)" `shouldBe` Right (BoolExpr And [LiteralExpr (BoolVal True), LiteralExpr (BoolVal False)], "")
        it "parses boolExpr: (and false false)" $
            parseStr "(and false false)" `shouldBe` Right (BoolExpr And [LiteralExpr (BoolVal False), LiteralExpr (BoolVal False)], "")
    describe "parse mathExpr" $ do
        it "parses mathExpr: (+ 3 4)" $
            parseStr "(+ 3 4)" `shouldBe` Right (MathExpr Add [LiteralExpr (IntVal 3), LiteralExpr (IntVal 4)], "")
        it "parses mathExpr: (- 5 4)" $
            parseStr "(- 5 4)" `shouldBe` Right (MathExpr Sub [LiteralExpr (IntVal 5), LiteralExpr (IntVal 4)], "")
        it "parses mathExpr: (* 6 7)" $
            parseStr "(* 6 7)" `shouldBe` Right (MathExpr Mul [LiteralExpr (IntVal 6), LiteralExpr (IntVal 7)], "")
        it "parses mathExpr: (div 4 2)" $
            parseStr "(div 4 2)" `shouldBe` Right (MathExpr Div [LiteralExpr (IntVal 4), LiteralExpr (IntVal 2)], "")
        it "parses mathExpr: (mod 4 2)" $
            parseStr "(mod 4 2)" `shouldBe` Right (MathExpr Mod [LiteralExpr (IntVal 4), LiteralExpr (IntVal 2)], "")
    describe "parse negateAtom/negateExpr/varExpr" $ do
        it "parses -x" $
            parseStr "-x" `shouldBe` Right (NegateExpr "x","")
        it "parses x" $
            parseStr "x" `shouldBe` Right (VarExpr "x","")
    describe "parse let" $ do
        it "parses (let (x 5) (+ x 3))" $
            parseStr "(let (x 5) (+ x 3))" `shouldBe` Right (LetExpr "x" (LiteralExpr (IntVal 5)) (MathExpr Add [VarExpr "x",LiteralExpr (IntVal 3)]),"")
        it "parses (let (x 70) (* x 2))" $
            parseStr "(let (x 70) (* x 2))" `shouldBe` Right (LetExpr "x" (LiteralExpr (IntVal 70)) (MathExpr Mul [VarExpr "x",LiteralExpr (IntVal 2)]),"")
    describe "parse if" $ do
        it "parses (if (< 4 5) true false)" $
            parseStr "(if (< 4 5) true false)" `shouldBe`Right (IfExpr (CompExpr Lt (LiteralExpr (IntVal 4)) (LiteralExpr (IntVal 5))) (LiteralExpr (BoolVal True)) (LiteralExpr (BoolVal False)),"")
        it "parses (if (< 5 4) 69 96)" $
            parseStr "(if (< 5 4) 69 96)" `shouldBe` Right (IfExpr (CompExpr Lt (LiteralExpr (IntVal 5)) (LiteralExpr (IntVal 4))) (LiteralExpr (IntVal 69)) (LiteralExpr (IntVal 96)),"")
    describe "parse lambda/apply" $ do
        it "parses (lambda (x) (+ x 3))" $
            parseStr "(lambda (x) (+ x 3))" `shouldBe` Right (LambdaExpr "x" (MathExpr Add [VarExpr "x",LiteralExpr (IntVal 3)]),"")
        it "parses (let (x (lambda (z) (+ z 5))) (+ x 3)) (apply is being tested here)" $
            parseStr "(let (x (lambda (z) (+ z 5))) (+ x 3))" `shouldBe` Right (LetExpr "x" (LambdaExpr "z" (MathExpr Add [VarExpr "z",LiteralExpr (IntVal 5)])) (MathExpr Add [VarExpr "x",LiteralExpr (IntVal 3)]),"")
        