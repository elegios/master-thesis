module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Char (generalCategory)
import System.Environment (getArgs)
import System.IO (hGetContents, withFile, IOMode(ReadMode))

import Lib

main :: IO ()
main = do
  [startSym, grammar, source] <- getArgs
  ambiguityParse startSym grammar source

ambiguityParse :: String -> String -> String -> IO ()
ambiguityParse startSym grammar source = do
  parser <- parseGrammar startSym <$> getConstructions grammar
  (results, report) <- parseFile parser source
  putStrLn $ show report
  case results of
    [] -> putStrLn "Nothing parsed"
    [res] -> putStrLn $ show res
    _ -> forM_ (ambiguities results) $ \(r, reprs) -> do
      putStrLn $ "Ambiguity: " ++ show r
      putStrLn . unlines $ show <$> reprs
  where
    parseFile parser source = withFile source ReadMode $ \f ->
      hGetContents f >>= evaluate . parser . tokenize . force
    getConstructions path = withFile path ReadMode $ \f -> do
      (parses, _) <- parseSyntax . tokenize <$> hGetContents f
      case parses of
        [p] -> return p
        [] -> error $ "Got no parses of grammar file \"" ++ path ++ "\""
        _ -> error $ "Got too many parses of grammar file \"" ++ path ++ "\""

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
