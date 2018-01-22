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
import qualified Data.Set as S

import Text.Earley (Grammar, Prod, Report(Report, unconsumed, expected))

import Lexer
import BootParser
import GrammarGenerator
import Types.Lexer (Token, range)
import Types.Construction
import Types.Ast
import Types.Result
import Types.ResolvedConstruction
import Types.GenSym (GenSym)
import Ambiguity
import ConstructionResolution
import Binding
import FullExpander
import Interpreter

type Constr s = Construction (FixNode s String)
type Production r a = Prod r String Token a

noImpl :: Production r b -> Grammar r (Production r a)
noImpl _ = return empty

main :: IO ()
main = do
  [startSym, coreGrammar, grammar, source] <- getArgs
  putStrLn "\nParsing coreGrammar"
  coreConstructions <- getConstructions noImpl coreGrammar
  putStrLn "\nResolving core Constructions"
  resolvedCoreConstructions <- resolveConstructions coreConstructions
  putStrLn "\nParsing grammar"
  constructions <- getConstructions (implementationGrammar coreConstructions) grammar
  putStrLn "\nResolving Constructions"
  resolvedConstructions <- resolveConstructions constructions
  let allResolvedConstructions = M.union resolvedConstructions resolvedCoreConstructions
  putStrLn "\nParsing source"
  node <- ambiguityParse constructions startSym source
  putStrLn "\nResolving source"
  node <- resolveSource resolvedConstructions node
  putStrLn . prettyShow $ node
  putStrLn "\nExpanding source"
  let coreEProgram = fullExpansion allResolvedConstructions node
  putStrLn $ showCoreProgram coreEProgram
  putStrLn "\nRemoving efuns"
  let coreUnresProgram = removeEFuns coreEProgram
  putStrLn $ showCoreProgram coreUnresProgram
  putStrLn "\nResolving again"
  coreProgram <- resolveSource allResolvedConstructions coreUnresProgram
  putStrLn $ showCoreProgram coreProgram
  putStrLn "\nInterpreting program"
  finalResult <- interpret coreProgram
  putStrLn $ "=> " ++ finalResult

resolveSource :: Gen pre => M.Map String ResolvedConstruction -> FixNode NoSplice pre -> IO (FixNode NoSplice GenSym)
resolveSource resolvedConstructions node = case resolveNames resolvedConstructions node of
  Data res -> return res
  Error es -> do
    putStrLn "Binding errors:"
    mapM_ (putStrLn . show) es
    error $ "Cannot continue"

ambiguityParse :: M.Map String (Constr _) -> String -> String -> IO (FixNode _ String)
ambiguityParse constructions startSym source = do
  (results, Report{expected, unconsumed}) <- parseFile parser source
  case results of
    [] -> do
      putStrLn $ "Got no parses of source file \"" ++ source ++ "\""
      putStrLn $ "Expected: " ++ (show . S.toList $ S.fromList expected)
      putStrLn $ "Unconsumed range: " ++ show (range unconsumed)
      putStrLn $ "Unconsumed: " ++ show unconsumed
      error $ "Cannot continue"
    [res] -> return res
    _ -> do
      forM_ (ambiguities results) $ \(r, reprs) -> do
        putStrLn $ "Ambiguity: " ++ show r
        putStrLn . unlines $ show <$> reprs
      error "Cannot continue"
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
  (parses, rep@Report{expected, unconsumed}) <- parseConstructions impl . tokenize . force <$> hGetContents f
  case parses of
    [p] -> return . M.fromList $ (Types.Construction.name &&& id) . addPrefix <$> p
    [] -> do
      putStrLn $ "Got no parses of grammar file \"" ++ path ++ "\""
      putStrLn $ "Expected: " ++ (show . S.toList $ S.fromList expected)
      putStrLn $ "Unconsumed range: " ++ show (range unconsumed)
      putStrLn $ "Unconsumed: " ++ show unconsumed
      error $ "Cannot continue"
    _ -> error $ "Got too many parses of grammar file \"" ++ path ++ "\""
  where
    addPrefix c@Construction{name} = c { Types.Construction.name = path ++ "#" ++ name }

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
