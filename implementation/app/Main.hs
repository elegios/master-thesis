module Main where

import Data.Char (generalCategory)
import System.Environment (getArgs)
import System.IO (hGetContents, withFile, IOMode(ReadMode))

import Lib

main :: IO ()
main = fullParse

fullParse :: IO ()
fullParse = do
  [startSym, grammar] <- getArgs
  parser <- parseGrammar startSym <$> getConstructions grammar
  interact $ prettier . parser . tokenize
  where
    getConstructions path = withFile path ReadMode $ \f -> do
      (parses, _) <- parseSyntax . tokenize <$> hGetContents f
      case parses of
        [p] -> return p
        [] -> error $ "Got no parses of grammar file \"" ++ path ++ "\""
        _ -> error $ "Got too many parses of grammar file \"" ++ path ++ "\""
    prettier (results, report) = unlines $ show report : (show <$> results)

printConstructions :: IO ()
printConstructions = interact $ show . parseSyntax . tokenize

printTokens :: IO ()
printTokens = interact $ unlines . map show . tokenize

checkCategory :: IO ()
checkCategory = interact $ unlines . map (show . generalCategory)
