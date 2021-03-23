module Language.Bolt.CST where

import GHC.Generics (Generic)
import Data.Hashable
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS

data Node
  = FunctionDeclaration {
      fnKeyword :: Node,
      name :: Node,
      openParen :: Node,
      params :: [Node],
      closeParen :: Node,
      returnTypeExpr :: Maybe (Node, Node),
      body :: Maybe Node,
      offsets :: (Int, Int)
    }
  | FunctionExpression {
      params :: [Node],
      returnTypeExpr :: Maybe (Node, Node),
      expr :: Node,
      offsets :: (Int, Int)
    }
  | Param {
      bindings :: Node,
      typeExpr :: Maybe (Node, Node),
      defaultValue :: Maybe (Node, Node),
      offsets :: (Int, Int)
    }
  | BindPattern {
      name :: Node,
      offsets :: (Int, Int)
    }
  | Block {
      openBrace :: Node,
      statements :: [Node],
      closeBrace :: Node,
      offsets :: (Int, Int)
    }
  | VariableDeclaration {
      letKeyword :: Node,
      typeExpr :: Maybe (Node, Node),
      bindings :: Node,
      valueExpr :: Maybe (Node, Node),
      offsets :: (Int, Int)
    }
  | TypeReferenceExpression {
      name :: Node,
      typeArgs :: Maybe (Node, Node, Node),
      offsets :: (Int, Int)
    }
  | ReferenceExpression {
      name :: Node,
      offsets :: (Int, Int)
    }
  | PrefixExpression {
      operator :: Node,
      operand :: Node,
      offsets :: (Int, Int)
    }
  | PostfixExpression {
      operand :: Node,
      operator :: Node,
      offsets :: (Int, Int)
    }
  | InfixExpression {
      left :: Node,
      operator :: Node,
      right :: Node,
      offsets :: (Int, Int)
    }
  | CallExpression {
      operator :: Node,
      openParen :: Node,
      operands :: [Node],
      closeParen :: Node,
      offsets :: (Int, Int)
    }
  | DoubleQuoteSign {
      offsets :: (Int, Int)
    }
  | ColonSign {
      offsets :: (Int, Int)
    }
  | PlusSign {
      offsets :: (Int, Int)
    }
  | MinusSign {
      offsets :: (Int, Int)
    }
  | AsteriskSign {
      offsets :: (Int, Int)
    }
  | FSlashSign {
      offsets :: (Int, Int)
    }
  | RArrowSign {
      offsets :: (Int, Int)
    }
  | OpenParen {
      offsets :: (Int, Int)
    }
  | CloseParen {
      offsets :: (Int, Int)
    }
  | OpenBrace {
      offsets :: (Int, Int)
    }
  | FunctionKeyword {
      offsets :: (Int, Int)
    }
  | CloseBrace {
      offsets :: (Int, Int)
    }
  | Identifier {
      text :: BS.ByteString,
      offsets :: (Int, Int)
    }
  | DecimalLiteral {
      value :: Integer,
      offsets :: (Int, Int)
    }
  | StringLiteral {
      openQuote :: Node,
      literals :: [Node],
      closeQuote :: Node,
      offsets :: (Int, Int)
    }
  | EscapeCharacter {
      escaped :: Char,
      offsets :: (Int, Int)
    }
  | LiteralText {
      contents :: BS.ByteString,
      offsets :: (Int, Int)
    }
  | LiteralInterpolation {
      expr :: Node,
      offsets :: (Int, Int)
    }
  | SourceFile {
      elements :: [Node],
      offsets :: (Int, Int)
    }
  | ExpressionStatement {
      expr :: Node,
      offsets :: (Int, Int)
    }
  | ReturnStatement {
      returnKeyword :: Node,
      expr :: Node,
      offsets :: (Int, Int)
    }
  deriving (Eq, Ord, Show, Generic)

data NodeType
  = TExpression
  | TSourceFile
  | TSourceElement
  | TFunctionBodyElement
  | TFunctionDeclaration
  | TReturnStatement

instance Hashable Node

instance JSON.ToJSON BS.ByteString where
  toJSON s = JSON.String $ TE.decodeUtf8 s

instance JSON.ToJSON Node where
  toEncoding = JSON.genericToEncoding opts
    where
      opts = JSON.defaultOptions {
          JSON.sumEncoding = JSON.TaggedObject {
            JSON.tagFieldName = "type",
            JSON.contentsFieldName = "value"
          }
        }

startOffset :: Node -> Int
startOffset = fst . offsets

endOffset :: Node -> Int
endOffset = snd . offsets

