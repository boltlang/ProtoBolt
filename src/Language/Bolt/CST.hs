module Language.Bolt.CST where

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
      valueExpr :: Maybe (Node, Node)
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
  deriving (Eq, Ord, Show)

startOffset :: Node -> Int
startOffset = fst . offsets

endOffset :: Node -> Int
endOffset = snd . offsets
