{-# LANGUAGE DeriveGeneric #-}

module Language.Bolt.AST where

import GHC.Generics (Generic)
import Data.Hashable
import qualified Data.ByteString as BS

import Language.Bolt.Common (Value)

data AST
  = Lam BS.ByteString AST
  | App AST AST
  | Ref BS.ByteString
  | Lit Value
  | Let BS.ByteString AST AST
  | If AST AST AST
  | Return AST
  deriving (Eq, Show, Ord, Generic)

instance Hashable AST

