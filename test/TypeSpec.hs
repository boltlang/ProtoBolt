module TypeSpec where

import Test.Hspec

import Language.Bolt.Common
import qualified Language.Bolt.TypeEnv as TE
import Language.Bolt.Type
import Language.Bolt.Infer
import Language.Bolt.AST

spec = do
  describe "a Core type checker" $ do
    it "can infer the type of a literal expression" $ do
      inferExpr TE.empty (Lit (VInt 1)) `shouldBe` Right (Forall [] intType)
      inferExpr TE.empty (Lit (VInt 2)) `shouldBe` Right (Forall [] intType)
      inferExpr TE.empty (Lit (VInt 3)) `shouldBe` Right (Forall [] intType)
      inferExpr TE.empty (Lit (VText "foo")) `shouldBe` Right (Forall [] stringType)
      inferExpr TE.empty (Lit (VText "a")) `shouldBe` Right (Forall [] stringType)
      inferExpr TE.empty (Lit (VText "b")) `shouldBe` Right (Forall [] stringType)
    it "can infer the type of a function application" $ do
      inferExpr TE.empty (Let "f" (Lam "x" (Ref "x")) (App (Ref "f") (Lit (VInt 1))))
         `shouldBe` Right (Forall [] intType)
      inferExpr TE.empty (Let "f" (Lam "x" (Ref "x")) (App (Ref "f") (Lit (VText "foo"))))
         `shouldBe` Right (Forall [] stringType)
