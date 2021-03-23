module Language.Bolt.Parser where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Char
import Control.Applicative hiding (many, some)
import Data.Void (Void)
import Control.Monad (void, MonadPlus)
import Debug.Trace (trace)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.Bolt.CST
import Language.Bolt.Compiler (Compiler)

type Parser a = ParsecT Void T.Text Compiler a

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

rword :: T.Text -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [T.Text]
rws = [
  "if",
  "else",
  "while",
  "for",
  "loop",
  "return",
  "yield",
  "effect",
  "trait",
  "impl",
  "pub",
  "fn",
  "let",
  "mod"
  ]

semi = (lookAhead (char '}') <|> char ';') *> sc

semis :: Parser ()
semis
  = takeWhileP Nothing (==';') *> sc >>= mempty

pFile :: Parser Node
pFile
  = do p1 <- getOffset
       sc
       els <- many $ (pDeclaration <|> pStatement) <* semis
       p2 <- getOffset
       eof
       return $ SourceFile els (p1, p2)

pDeclaration :: Parser Node
pDeclaration
  = pFunctionDeclaration

pToken :: T.Text -> ((Int, Int) -> Node) -> Parser Node
pToken str mkNode
  = do p1 <- getOffset
       string str
       p2 <- getOffset
       sc
       return $ mkNode (p1, p2)

pOpenParen       = pToken "("  OpenParen
pCloseParen      = pToken ")"  CloseParen
pOpenBrace       = pToken "{"  OpenBrace
pCloseBrace      = pToken "}"  CloseBrace
pFunctionKeyword = pToken "fn" FunctionKeyword
pRarrowSign      = pToken "->" RArrowSign
pColon           = pToken ":"  ColonSign
pPlusSign        = pToken "+"  PlusSign
pMinusSign       = pToken "-"  MinusSign
pAsteriskSign    = pToken "*"  AsteriskSign
pFSlashSign      = pToken "/"  FSlashSign

pTypeExpr
  = pTypeReferenceExpression

pTypeReferenceExpression
  = do p1 <- getOffset
       name <- pIdentifier
       p2 <- getOffset
       return $ TypeReferenceExpression name Nothing (p1, p2)

pFunctionBodyElement
  = (pDeclaration <|> pStatement) <* semis

pBlock :: Parser Node
pBlock
  = do p1 <- getOffset
       openBrace <- pOpenBrace
       stmts <- many pFunctionBodyElement
       closeBrace <- pCloseBrace
       p2 <- getOffset
       return $ Block openBrace stmts closeBrace (p1, p2)

pFunctionDeclaration :: Parser Node
pFunctionDeclaration
  = do p1 <- getOffset
       fnKeyword <- pFunctionKeyword
       name <- pIdentifier
       openParen <- pOpenParen
       let params = []
       closeParen <- pCloseParen
       returnTypeExpr <- optional $ (,) <$> pRarrowSign <*> pTypeExpr
       body <- optional pBlock
       p2 <- getOffset
       return $ FunctionDeclaration
          fnKeyword
          name
          openParen
          params
          closeParen
          returnTypeExpr
          body
          (p1, p2)

pStatement :: Parser Node
pStatement
  = pExpressionStatement

pExpressionStatement :: Parser Node
pExpressionStatement
  = do p1 <- getOffset
       expr <- pExpression
       semi
       p2 <- getOffset
       return $ ExpressionStatement expr (p1, p2)

pIdentifier :: Parser Node
pIdentifier
  = do p1 <- getOffset
       text <- try p
       check text
       p2 <- getOffset
       sc
       return $ Identifier (TE.encodeUtf8 text) (p1, p2)
  where p = T.cons <$> pIdentStart <*> takeWhileP (Just "a letter or digit") isIdentPart
        check x = if x `elem` rws
                  then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                  else return ()
        pIdentStart = letterChar <|> char '_'
        isIdentPart '_' = True
        isIdentPart ch = isAlphaNum ch

pPrimaryExpression :: Parser Node
pPrimaryExpression
  =   char '(' *> sc *> pExpression <* sc <* char ')'
  <|> pStringLiteral
  <|> pDecimalLiteral
  <|> pReferenceExpression

data Operator
 = InfixL  (Parser Node)
 | InfixR  (Parser Node)
 | InfixN  (Parser Node)
 | Prefix  (Parser Node)
 | Postfix (Parser Node)
 | TernR   (Parser (Parser Node))

operators :: [[Operator]]
operators = [
    [ InfixL pAsteriskSign
    , InfixL pFSlashSign ]
  , [ InfixL pPlusSign
    , InfixL pMinusSign ]
  ]

pExpression
  = makeExprParser operators pPrimaryExpression

makeExprParser :: [[Operator]] -> Parser Node -> Parser Node
makeExprParser table pTerm
  = foldl addPrecLevel pTerm table
  where
    addPrecLevel :: Parser Node -> [Operator] -> Parser Node
    addPrecLevel pTerm ops
      = pUnary >>= \x -> choice [pInfixR x, pInfixL x, pInfixN x, return x]
      where
        (r, l, n, pre, post ) = foldr splitOp ([], [], [], [], []) ops
        pUnary
          = do p1 <- getOffset
               preOp <- optional $ choice pre
               p2 <- getOffset
               x <- pTerm
               p3 <- getOffset
               postOp <- optional $ choice post
               p4 <- getOffset
               let x' = maybe x (\op -> PrefixExpression op x (p1, p3)) preOp
               return $ maybe x' (\op -> PostfixExpression x' op (p2, p4)) postOp
        pInfixR left
          = do op <- choice r
               right <- pUnary >>= \r -> pInfixR r <|> return r
               return $ InfixExpression left op right (startOffset left, endOffset right)
        pInfixL left
          = do op <- choice l
               right <- pUnary
               let e = InfixExpression left op right (startOffset left, endOffset right)
               pInfixL e <|> return e
        pInfixN left
          = do op <- choice n
               right <- pUnary
               return $ InfixExpression left op right (startOffset left, endOffset right)

splitOp (InfixR  op) (r, l, n, pre, post) = (op:r, l, n, pre, post)
splitOp (InfixL  op) (r, l, n, pre, post) = (r, op:l, n, pre, post)
splitOp (InfixN  op) (r, l, n, pre, post) = (r, l, op:n, pre, post)
splitOp (Prefix  op) (r, l, n, pre, post) = (r, l, n, op:pre, post)
splitOp (Postfix op) (r, l, n, pre, post) = (r, l, n, pre, op:post)

parseInt :: Integer -> String -> Integer
parseInt n digits
  = sum $ map f $ zip [0..] $ map fromDecimal digits
  where f (i, k) = k * n ^ i

fromDecimal :: Char -> Integer
fromDecimal '0' = 0
fromDecimal '1' = 1
fromDecimal '2' = 2
fromDecimal '3' = 3
fromDecimal '4' = 4
fromDecimal '5' = 5
fromDecimal '6' = 6
fromDecimal '7' = 7
fromDecimal '8' = 8
fromDecimal '9' = 9

pDecimalLiteral :: Parser Node
pDecimalLiteral
  = do p1 <- getOffset
       value <- takeWhile1P (Just "a decimal digit") isDigit
       p2 <- getOffset
       return $ DecimalLiteral (parseInt 10 $ T.unpack value) (p1, p2)

pStringLiteralElement
  =   pEscapeSeq
  <|> pInterpolation
  <|> pLiteralText

unescape 'a' = Just '\a'
unescape 'b' = Just '\b'
unescape 'f' = Just '\f'
unescape 'n' = Just '\n'
unescape 'r' = Just '\r'
unescape 't' = Just '\t'
unescape 'v' = Just '\v'
unescape '0' = Just '\0'
unescape _   = Nothing

pInterpolation
  = do p1 <- getOffset
       char '{'
       sc
       expr <- pExpression
       sc
       char '}'
       p2 <- getOffset
       return $ LiteralInterpolation expr (p1, p2)

pReferenceExpression
  = do name <- pIdentifier
       return $ ReferenceExpression name (startOffset name, endOffset name)

pEscapeSeq :: Parser Node
pEscapeSeq
  = do p1 <- getOffset
       char '\\'
       ch <- anySingle
       p2 <- getOffset
       return $ case unescape ch of
         Just ch' -> EscapeCharacter ch' (p1, p2)
         Nothing -> LiteralText (BS.singleton ch) (p1, p2)

pLiteralText :: Parser Node
pLiteralText
  = do p1 <- getOffset
       text <- takeWhile1P Nothing (/='"')
       p2 <- getOffset
       return $ LiteralText (TE.encodeUtf8 text) (p1, p2)

pStringLiteral
  = do p1 <- getOffset
       char '"'
       p2 <- getOffset
       els <- many pStringLiteralElement
       p3 <- getOffset
       char '"'
       p4 <- getOffset
       sc
       return $ StringLiteral (DoubleQuoteSign (p1, p2)) els (DoubleQuoteSign (p3, p4)) (p1, p4)

