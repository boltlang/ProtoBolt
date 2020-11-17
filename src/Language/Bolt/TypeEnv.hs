module Language.Bolt.TypeEnv where

import qualified Data.ByteString as BS
import qualified Data.Map as Map

import Language.Bolt.Type

data TypeEnv
  = TypeEnv { types :: Map.Map BS.ByteString Scheme }
  deriving (Eq, Show)

empty :: TypeEnv
empty = TypeEnv Map.empty

extend :: (BS.ByteString, Scheme) -> TypeEnv -> TypeEnv
extend (n, s) env = env { types = Map.insert n s (types env) }

delete :: BS.ByteString -> TypeEnv -> TypeEnv
delete n env = env { types = Map.delete n (types env) }

lookup :: BS.ByteString -> TypeEnv -> Maybe Scheme
lookup n (TypeEnv tys) = Map.lookup n tys

merge :: TypeEnv -> TypeEnv -> TypeEnv
merge (TypeEnv a) (TypeEnv b) = TypeEnv (Map.union a b)

mergeN :: [TypeEnv] -> TypeEnv
mergeN = foldl merge empty

singleton :: BS.ByteString -> Scheme -> TypeEnv
singleton n s = TypeEnv (Map.singleton n s)

keys :: TypeEnv -> [BS.ByteString]
keys (TypeEnv env) = Map.keys env

fromList :: [(BS.ByteString, Scheme)] -> TypeEnv
fromList xs = TypeEnv $ Map.fromList xs

instance Semigroup TypeEnv where
  (<>) = merge

instance Monoid TypeEnv where
  mempty = empty

