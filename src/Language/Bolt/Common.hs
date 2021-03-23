
module Language.Bolt.Common where

import GHC.Generics (Generic)
import Data.Hashable
import qualified Data.Text as T

data Value
  = VText  T.Text
  | VInt   Integer
  | VRat   Rational
  deriving (Show, Eq, Ord, Generic)

instance Hashable Value
