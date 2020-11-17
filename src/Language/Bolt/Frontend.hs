module Language.Bolt.Frontend where

import Control.Monad.State (modify)
import Text.Megaparsec (runParserT)
import qualified Data.Text as T

import Language.Bolt.Compiler
import Language.Bolt.Parser (pFile)

loadFile :: String -> T.Text -> Compiler ()
loadFile filename contents
  = do result <- runParserT pFile filename contents
       case result of
         Left errorBundle -> modify $ addDiagnostic $ ParseDiagnostic errorBundle
         Right node -> modify $ addSourceFile filename node

