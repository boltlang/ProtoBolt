module Language.Bolt.AST where

import qualified Data.ByteString as BS

import Language.Bolt.Common (Value)

data AST
  = Lam BS.ByteString AST
  | App AST AST
  | Ref BS.ByteString
  | Lit Value
  | Let BS.ByteString AST AST
  | If AST AST AST
