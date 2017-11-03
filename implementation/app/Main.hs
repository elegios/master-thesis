module Main where

import Control.DeepSeq (force)

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Char (generalCategory)
import System.Environment (getArgs)
import System.IO (hGetContents, withFile, IOMode(ReadMode))

import Lexer
import BootParser
import GrammarGenerator
import Types.Construction
import Types.Ast
import Ambiguity
import Binding

main :: IO ()
main = do
  [startSym, grammar, source] <- getArgs
  constructions <- getConstructions grammar
  mNode <- ambiguityParse constructions startSym source
  case resolve constructions <$> mNode of
    Nothing -> return ()
    Just (Data res) -> putStrLn $ prettyShow res
    Just (Error es) -> putStrLn "Binding errors:" >> mapM_ (putStrLn . show) es

ambiguityParse :: [Construction] -> String -> String -> IO (Maybe (NodeI String))
ambiguityParse constructions startSym source = do
  (results, report) <- parseFile parser source
  putStrLn $ show report
  case results of
    [] -> putStrLn "Nothing parsed" >> return Nothing
    [res] -> return $ Just res
    _ -> do
      forM_ (ambiguities results) $ \(r, reprs) -> do
        putStrLn $ "Ambiguity: " ++ show r
        putStrLn . unlines $ show <$> reprs
      return Nothing
  where
    parser = parseGrammar startSym constructions
    parseFile parser source = withFile source ReadMode $ \f ->
      hGetContents f >>= evaluate . parser . tokenize . force

getConstructions :: FilePath -> IO [Construction]
getConstructions path = withFile path ReadMode $ \f -> do
  (parses, rep) <- parseSyntax . tokenize <$> hGetContents f
  case parses of
    [p] -> return p
    [] -> error $ "Got no parses of grammar file \"" ++ path ++ "\": " ++ show rep
    _ -> error $ "Got too many parses of grammar file \"" ++ path ++ "\""

fullParse :: IO ()
fullParse = do
  [startSym, grammar] <- getArgs
  parser <- parseGrammar startSym <$> getConstructions grammar
  interact $ prettier . parser . tokenize
  where
    prettier (results, report) = unlines $ show report : (show <$> results)

printConstructions :: IO ()
printConstructions = interact $ show . parseSyntax . tokenize

printTokens :: IO ()
printTokens = interact $ unlines . map show . tokenize

checkCategory :: IO ()
checkCategory = interact $ unlines . map (show . generalCategory)
