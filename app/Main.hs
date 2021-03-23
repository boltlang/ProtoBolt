module Main where

import Options.Applicative
import Control.Monad (mapM_)

import qualified Data.ByteString.Lazy.Char8 as Char8 (putStrLn)
import qualified Data.Text.IO as TIO
import Data.Aeson (encode)

import Language.Bolt.Parser (pFile)
import Language.Bolt.CST (NodeType)
import Language.Bolt.Compiler
import Language.Bolt.Frontend
import Language.Bolt.Pretty

data Options
  = ParseDumpOptions {
      files :: [String],
      nodeType :: String
    }
  | ExecOptions {
      files :: [String]
    }

fileVar = argument str $
     metavar "FILE"
  <> help "A Bolt file to parse"

nodeTypeVar = strOption $
     long "node-type"
  <> short 't'
  <> help "What kind of node to parse"
  <> showDefault
  <> value "File"
  <> metavar "NODE-TYPE"

commands :: Parser Options
commands = subparser $
     command "parse-dump" (info (ParseDumpOptions <$> some fileVar <*> nodeTypeVar) (progDesc "Dump parsed nodes in JSON format"))
  <> command "exec"       (info (ExecOptions      <$> some fileVar) (progDesc "Execute the given Bolt program"))

main
  = cli =<< execParser opts
  where
    opts
      = info (commands <**> helper) $
         fullDesc
      <> progDesc "The compiler for the Bolt programming language"

cli :: Options -> IO ()

cli ParseDumpOptions { files, nodeType }
  = do xs <- mapM f files
       mapM_ (Char8.putStrLn . encode) xs
  where f fname
          = do s <- TIO.readFile fname
               let (diags, x) = parse pFile fname s
               return x

cli ExecOptions { files }
  = putStrLn "Executing!"

-- main :: IO ()
-- main
--   = do args <- getArgs
--        files <- mapM TIO.readFile args
--        let (diags, _) = runCompiler $ forM (zip args files) $ uncurry loadFile
--        putStrLn $ pretty diags
--        return ()

