module Language.Bolt.Compiler where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Void (Void)
import Control.Monad.State
import Control.Monad.Except
import Text.Megaparsec.Error (ParseErrorBundle)

import Language.Bolt.Type
import Language.Bolt.CST

data CompilerError
  = ParseError
  | TypeError
    deriving (Eq, Show)

data Diagnostic
  = ParseDiagnostic {
      errorBundle :: ParseErrorBundle T.Text Void
    }
  | InfiniteTypeDiagnostic {
      varName :: BS.ByteString,
      ty :: Type
    }

data CompilerState = CompilerState {
    diagnostics :: [Diagnostic],
    sourceFiles :: Map.Map FilePath Node
  }

type Compiler = StateT CompilerState (Except CompilerError)

addSourceFile filename node s@CompilerState { sourceFiles }
  = s { sourceFiles = Map.insert filename node sourceFiles }

addDiagnostic d s@CompilerState { diagnostics }
  = s { diagnostics = d : diagnostics }

runCompiler :: Compiler a -> Either CompilerError a
runCompiler
  = runExcept . flip evalStateT s
  where s = CompilerState {
              sourceFiles = Map.empty,
              diagnostics = []
            }

