module Language.Bolt.Frontend where

import Control.Monad.State (modify)
import Text.Megaparsec (runParserT)
import qualified Data.Text as T

import Language.Bolt.Compiler
import Language.Bolt.CST
import Language.Bolt.Parser (Parser, pFile)

loadFile :: String -> T.Text -> Compiler (Maybe Node)
loadFile filename contents
  = do result <- runParserT pFile filename contents
       case result of
         Left errorBundle -> do
           modify $ addDiagnostic $ ParseDiagnostic errorBundle
           return Nothing
         Right node -> do
           modify $ addSourceFile filename node
           return $ Just node

parse :: Parser a -> String -> T.Text -> (DiagnosticBundle, Maybe a)
parse p fname s
  = case runCompiler $ runParserT p fname s of
      (diags, Nothing) ->
        (diags, Nothing)
      (DiagnosticBundle diags, Just (Left errorBundle)) ->
        (DiagnosticBundle $ ParseDiagnostic errorBundle : diags, Nothing)
      (diags, Just (Right x)) ->
        (diags, Just x)

