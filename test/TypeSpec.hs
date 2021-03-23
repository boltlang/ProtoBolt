module TypeSpec where

import Test.Hspec

import Language.Bolt.Common
import qualified Language.Bolt.TypeEnv as TE
import qualified Language.Bolt.TypeEnv as TypeEnv
import Language.Bolt.Type
import Language.Bolt.Infer
import Language.Bolt.Compiler
import Language.Bolt.CST

testInfer x
  = snd $ runCompiler $ inferExpr TypeEnv.empty x

spec = do

  describe "a type checker for the Bolt language" $ do

    it "can type-check a string expression" $ do
      testInfer 
        (StringLiteral
          (DoubleQuoteSign (0, 0))
          [ LiteralText "Hello!" (1, 7) ]
          (DoubleQuoteSign (0, 0))
          (0, 8))
      `shouldBe` Just 
      (Forall [] stringType)

