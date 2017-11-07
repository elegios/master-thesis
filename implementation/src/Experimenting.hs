{-# LANGUAGE Rank2Types #-}

module Experimenting where

import Control.DeepSeq (force)

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Control.Applicative (empty)
import Control.Arrow ((&&&))
import Data.Char (generalCategory)
import System.Environment (getArgs)
import System.IO (hGetContents, withFile, IOMode(ReadMode))

import qualified Data.Map as M

import Text.Earley (Grammar, Prod, Report)

import Lexer
import BootParser
import GrammarGenerator
import Types.Lexer (Token)
import Types.Construction
import Types.Ast
import Ambiguity
import Binding

type Constr = Construction (NodeI String)
type Production r a = Prod r String Token a

-- TODO: reconnect all the things, add dependency resolution

noImpl :: Grammar r (Production r a)
noImpl = return empty

main :: IO ()
main = do
  [startSym, coreGrammar, grammar, source] <- getArgs
  coreConstructions <- getConstructions noImpl coreGrammar
  constructions <- getConstructions (implementationGrammar coreConstructions) grammar
  mNode <- ambiguityParse constructions startSym source
  case resolve constructions <$> mNode of
    Nothing -> return ()
    Just (Data res) -> putStrLn $ prettyShow res
    Just (Error es) -> putStrLn "Binding errors:" >> mapM_ (putStrLn . show) es

ambiguityParse :: M.Map String Constr -> String -> String -> IO (Maybe (NodeI String))
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
    parser = parseWithGrammar startSym constructions
    parseFile parser source = withFile source ReadMode $ \f ->
      hGetContents f >>= evaluate . parser . tokenize . force

getConstructions :: (forall r. Grammar r (Production r (NodeI String))) -> FilePath -> IO (M.Map String Constr)
getConstructions impl path = withFile path ReadMode $ \f -> do
  (parses, rep) <- parseConstructions impl . tokenize . force <$> hGetContents f
  case parses of
    [p] -> return . M.fromList $ (Types.Construction.name &&& id) <$> p
    [] -> error $ "Got no parses of grammar file \"" ++ path ++ "\": " ++ show rep
    _ -> error $ "Got too many parses of grammar file \"" ++ path ++ "\""

fullParse :: IO ()
fullParse = do
  [startSym, grammar] <- getArgs
  parser <- parseWithGrammar startSym <$> getConstructions noImpl grammar
  interact $ prettier . parser . tokenize
  where
    prettier (results, report) = unlines $ show report : (show <$> results)

printConstructions :: IO ()
printConstructions = interact $
  (show :: ([[Constr]], Report String [Token]) -> String) . parseConstructions noImpl . tokenize

printTokens :: IO ()
printTokens = interact $ unlines . map show . tokenize

checkCategory :: IO ()
checkCategory = interact $ unlines . map (show . generalCategory)
