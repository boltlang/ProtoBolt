module Language.Bolt.Compiler where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Data.Void (Void)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

import Language.Bolt.Type
import Language.Bolt.CST
import Language.Bolt.AST

data CompilerError
  = HasFatalDiagnosticError
  deriving (Eq, Show, Ord)

data ParseItem
  = ParseLabel T.Text
  | ParseText T.Text
  | ParseEOF
  deriving (Show)

data Diagnostic
  = ParseError {
      fName :: String,
      fLine :: Int,
      fColumn :: Int,
      actual :: Maybe ParseItem,
      expected :: [ParseItem]
    }
  | InfiniteTypeError {
      typeVar :: TVar,
      ty :: Type
    }
  | OccursCheckError {
      varName :: BS.ByteString
    }
  | UnboundVariableError {
      varName :: BS.ByteString
    }
  | UnificationFailError {
      leftType :: Type,
      rightType :: Type
    }
  deriving (Show)

data CompilerState = CompilerState {
    sourceFiles :: Map.Map FilePath Node
  }

type Compiler = ExceptT CompilerError (StateT CompilerState (Writer [Diagnostic]))

addSourceFile :: String -> Node -> Compiler ()
addSourceFile filename node 
  = modify $ \s@CompilerState { sourceFiles } -> s {
      sourceFiles = Map.insert filename node sourceFiles
    }

addDiagnostic :: Diagnostic -> Compiler ()
addDiagnostic d
  = tell [ d ]

abortCompilation :: Compiler ()
abortCompilation
  = throwError HasFatalDiagnosticError

runCompiler :: Compiler a -> ([Diagnostic], Maybe a)
runCompiler c
  = case x of
      (Left _, s) -> (ds, Nothing)
      (Right val, s) -> (ds, Just val)
  where (x, ds) = (runWriter . flip runStateT s . runExceptT) c
        s = CompilerState {
              sourceFiles = Map.empty
            }

