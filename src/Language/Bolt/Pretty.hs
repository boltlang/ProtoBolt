module Language.Bolt.Pretty where

import Data.List (intercalate)
import Text.Megaparsec.Error (errorBundlePretty)

import Language.Bolt.Compiler

class Pretty a where
  pretty :: a -> String

instance Pretty DiagnosticBundle where
  pretty (DiagnosticBundle ds) = intercalate "\n\n" $ map pretty ds

instance Pretty Diagnostic where
  pretty (ParseDiagnostic errorBundle) = "error: " <> errorBundlePretty errorBundle

