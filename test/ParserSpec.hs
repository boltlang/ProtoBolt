module ParserSpec where

import qualified Data.Text as T
import Debug.Trace
import Test.Hspec
import Text.Megaparsec
import Language.Bolt.CST
import Language.Bolt.Parser
import Language.Bolt.Compiler

testParse :: Parser a -> T.Text -> Either CompilerError a
testParse p s
  = case runCompiler $ runParserT p "#<anonumous>" s of
      Left e -> Left e
      Right (Left c) -> trace (errorBundlePretty c) (Left ParseError)
      Right (Right v) -> Right v

mkRefNode name offsets
  = ReferenceExpression (Identifier name offsets) offsets

spec :: Spec
spec = do
  describe "a Bolt language parser" $ do
    it "can parse an identifier" $ do
      testParse pIdentifier "foo" `shouldBe` Right (Identifier "foo" (0, 3))
      testParse pIdentifier "bar" `shouldBe` Right (Identifier "bar" (0, 3))
      testParse pIdentifier "bax123" `shouldBe` Right (Identifier "bax123" (0, 6))
      testParse pIdentifier "bax__1" `shouldBe` Right (Identifier "bax__1" (0, 6))
    it "can parse a statement block" $ do
      testParse pBlock "{}" `shouldBe` Right (Block (OpenBrace (0, 1)) [] (CloseBrace (1, 2)) (0, 2))
    it "can parse a function declaration" $ do
      testParse pFunctionDeclaration "fn foo() -> Int" `shouldBe`
        Right (FunctionDeclaration
          (FunctionKeyword (0, 2))
          (Identifier "foo" (3, 6))
          (OpenParen (6, 7))
          []
          (CloseParen (7, 8))
          (Just $ ((RArrowSign (9, 11)), (TypeReferenceExpression (Identifier "Int" (12, 15)) Nothing (12, 15))))
          Nothing
          (0, 15))
      testParse pFunctionDeclaration "fn foo() {}" `shouldBe`
        Right (FunctionDeclaration
          (FunctionKeyword (0, 2))
          (Identifier "foo" (3, 6))
          (OpenParen (6, 7))
          []
          (CloseParen (7, 8))
          Nothing
          (Just $ Block (OpenBrace (9, 10)) [] (CloseBrace (10, 11)) (9, 11))
          (0, 11))
    it "can parse a simple reference expression" $ do
      testParse pExpression "foo" `shouldBe`
        Right (ReferenceExpression
          (Identifier "foo" (0, 3))
          (0, 3))
    it "can correctly parse operator precedence" $ do
      testParse pExpression "a + b / c * d - e" `shouldBe` 
        Right (InfixExpression
          (InfixExpression
            (mkRefNode "a" (0, 1))
            (PlusSign (2, 3))
              (InfixExpression
                (InfixExpression
                  (mkRefNode "b" (4, 5))
                  (FSlashSign (6, 7))
                  (mkRefNode "c" (8, 9))
                  (4, 9))
                (AsteriskSign (10, 11))
                (mkRefNode "d" (12, 13))
                (4, 13))
            (0, 13))
          (MinusSign (14, 15))
          (mkRefNode "e" (16, 17))
          (0, 17))

