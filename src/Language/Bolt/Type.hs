module Language.Bolt.Type where

import qualified Data.ByteString as BS

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon BS.ByteString
  | TArr Type Type
  | TAny
  deriving (Show, Eq, Ord)

data Scheme
  = Forall [TVar] Type
  deriving (Show, Eq, Ord)

-- data TypeScheme
--   = Mono Type
--   | Poly [TVar] Constraint Type
--   deriving (Show, Eq, Ord)

intType :: Type
intType = TCon "Int"

boolType :: Type
boolType = TCon "Bool"

stringType :: Type
stringType = TCon "String"

