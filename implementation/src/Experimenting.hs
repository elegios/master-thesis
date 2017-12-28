{-# LANGUAGE Rank2Types, PartialTypeSignatures #-}

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

import Text.Earley (Grammar, Prod)

import Lexer
import BootParser
import GrammarGenerator
import Types.Lexer (Token)
import Types.Construction
import Types.Ast
import Types.Result
import Types.ResolvedConstruction
import Ambiguity
import ConstructionResolution
import Binding

type Constr s = Construction (FixNode s String)
type Production r a = Prod r String Token a

noImpl :: Production r b -> Grammar r (Production r a)
noImpl _ = return empty

main :: IO ()
main = do
  [startSym, coreGrammar, grammar, source] <- getArgs
  putStrLn "Parsing coreGrammar"
  coreConstructions <- getConstructions noImpl coreGrammar
  putStrLn "Parsing grammar"
  constructions <- getConstructions (implementationGrammar coreConstructions) grammar
  putStrLn "Parsing source"
  mNode <- ambiguityParse constructions startSym source
  putStrLn "Resolving Constructions"
  resolvedConstructions <- resolveConstructions constructions
  forM_ (M.toList resolvedConstructions) $ putStrLn . show
  putStrLn "Resolving source"
  case resolveNames resolvedConstructions <$> mNode of
    Nothing -> return ()
    Just (Data res) -> putStrLn $ prettyShow res
    Just (Error es) -> putStrLn "Binding errors:" >> mapM_ (putStrLn . show) es

ambiguityParse :: M.Map String (Constr _) -> String -> String -> IO (Maybe (FixNode _ String))
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

resolveConstructions :: M.Map String (Constr _) -> IO (M.Map String ResolvedConstruction)
resolveConstructions constructions =
  case sequenceA $ ConstructionResolution.resolve <$> constructions of
    Data resolvedConstructions -> return resolvedConstructions
    Error es -> do
      putStrLn "Construction resolution errors: "
      mapM_ (putStrLn . show) es
      error $ "Cannot continue"

getConstructions :: (forall r. Production r (Splice (FixNode _ String)) -> Grammar r (Production r (FixNode _ String))) -> FilePath -> IO (M.Map String (Constr _))
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

printTokens :: IO ()
printTokens = interact $ unlines . map show . tokenize

checkCategory :: IO ()
checkCategory = interact $ unlines . map (show . generalCategory)
