module Language.Bolt.Compiler where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Data.Void (Void)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Text.Megaparsec.Error (ParseErrorBundle)

import Language.Bolt.Type
import Language.Bolt.CST
import Language.Bolt.AST

data CompilerError
  = HasFatalDiagnosticError
  deriving (Eq, Show, Ord)

newtype DiagnosticBundle
  = DiagnosticBundle [Diagnostic]
  deriving (Eq, Show)

instance Semigroup DiagnosticBundle where
  DiagnosticBundle a <> DiagnosticBundle b = DiagnosticBundle $ a <> b

instance Monoid DiagnosticBundle where
  mempty = DiagnosticBundle []

data Diagnostic
  = ParseDiagnostic {
      errorBundle :: ParseErrorBundle T.Text Void
    }
  | InfiniteTypeDiagnostic {
      varName :: BS.ByteString,
      ty :: Type
    }
  | OccursCheckDiagnostic {
      varName :: BS.ByteString
    }
  deriving (Eq, Show)

data CompilerState = CompilerState {
    diagnostics :: [Diagnostic],
    sourceFiles :: Map.Map FilePath Node,
    origNodes :: HashMap.HashMap AST Node
  }

type CompilerT a = ExceptT CompilerError (StateT CompilerState a)

type Compiler = CompilerT Identity

addSourceFile filename node s@CompilerState { sourceFiles }
  = s { sourceFiles = Map.insert filename node sourceFiles }

addDiagnostic d s@CompilerState { diagnostics }
  = s { diagnostics = d : diagnostics }

setOrigNode :: AST -> Node -> CompilerState -> CompilerState
setOrigNode ast cst s
  = s { origNodes = HashMap.insert ast cst (origNodes s) }

abort :: Compiler ()
abort
  = throwError HasFatalDiagnosticError

runCompiler :: Compiler a -> (DiagnosticBundle, Maybe a)
runCompiler c
  = case (flip runState s . runExceptT) c of
      (Left _, s) -> (DiagnosticBundle $ diagnostics s, Nothing)
      (Right x, s) -> (DiagnosticBundle $ diagnostics s, Just x)
  where s = CompilerState {
              sourceFiles = Map.empty,
              diagnostics = [],
              origNodes = HashMap.empty
            }

