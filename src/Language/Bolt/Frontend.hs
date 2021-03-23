module Language.Bolt.Frontend where

import Control.Monad (forM_)
import qualified Data.Set as Set
import Text.Megaparsec hiding (parse)
import Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Language.Bolt.Compiler
import Language.Bolt.CST
import Language.Bolt.Parser (Parser, pFile)

loadFile :: String -> T.Text -> Compiler (Maybe Node)
loadFile fname contents
  = do node <- parse pFile fname contents
       addSourceFile fname $ fromMaybe SourceFile { elements = [], offsets = (0, 0) } node
       return node

initPosState s =
  PosState
    { pstateInput = s,
      pstateOffset = 0,
      pstateSourcePos = initialPos "",
      pstateTabWidth = defaultTabWidth,
      pstateLinePrefix = ""
    }

fromErrorItem :: ErrorItem Char -> ParseItem
fromErrorItem (Tokens ts) = ParseText $ T.pack $ NonEmpty.toList ts
fromErrorItem (Label cs) = ParseText $ T.pack $ NonEmpty.toList cs
fromErrorItem EndOfInput = ParseEOF

parse :: Parser a -> String -> T.Text -> Compiler (Maybe a)
parse p fname s
  = do runParserT p fname s >>= \case
         Left ParseErrorBundle { bundleErrors, bundlePosState } -> do
           forM_ (fst $ attachSourcePos errorOffset bundleErrors bundlePosState) addParseError
           return Nothing
         Right x -> return $ Just x
  where addParseError (TrivialError offset actual expected, pos)
          = addDiagnostic $ ParseError sourceName (unPos sourceLine) (unPos sourceColumn) (fmap fromErrorItem actual) (map fromErrorItem $ Set.elems expected)
          where SourcePos { sourceName, sourceLine, sourceColumn } = pos

