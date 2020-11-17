
module Language.Bolt.Common where

import qualified Data.Text as T

data Value
  = VText  T.Text
  | VInt   Integer
  | VRat   Rational
  deriving (Show, Eq, Ord)


