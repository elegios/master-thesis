{-# LANGUAGE Rank2Types, PartialTypeSignatures #-}

module Experimenting where

import Control.DeepSeq (NFData, force)
import Data.Time (getCurrentTime, diffUTCTime)

import Control.Exception (evaluate)
import Control.Monad (forM_, void, foldM)
import Control.Applicative (empty)
import Control.Arrow ((&&&))
import Data.Char (generalCategory)
import System.Environment (getArgs)
import System.IO (hGetContents, withFile, IOMode(ReadMode), hFlush, stdout)

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
import ImplChecker
import Binding
import FullExpander
import Interpreter

type Constr s = Construction (FixNode s String)
type Production r a = Prod r String Token a

noImpl :: Production r b -> Grammar r (Production r a)
noImpl _ = return empty

isOption :: String -> Bool
isOption "-" = False
isOption ('-' : _) = True
isOption _ = False

hasOption :: String -> IO Bool
hasOption opt = any (\a -> a == opt || a == shortOpt) <$> getArgs
  where
    shortOpt = "-" ++ formatShortOpt opt
    formatShortOpt "" = ""
    formatShortOpt ('-' : cs@('-':_)) = formatShortOpt cs
    formatShortOpt ('-' : c : cs) = c : formatShortOpt cs
    formatShortOpt (_ : cs) = formatShortOpt cs

main :: IO ()
main = do
  bench <- hasOption "--bench"
  if bench
    then benchOperations
    else normalOperations

normalOperations :: IO ()
normalOperations = do
  startSym : coreGrammar : rest <- filter (not . isOption) <$> getArgs
  let Just (grammars, source) = unsnoc rest
  ignoreExpansionCheckingFailure <- hasOption "--ignore-expansion-check"
  putStrLn "\nParsing coreGrammar"
  coreConstructions <- getConstructions noImpl coreGrammar
  putStrLn "\nResolving core Constructions"
  resolvedCoreConstructions <- resolveConstructions coreConstructions
  (allConstructions, (langConstructions, allResolvedConstructions))
    <- foldM grammar (coreConstructions, (coreConstructions, resolvedCoreConstructions)) grammars
  putStrLn "\nChecking Implementations"
  implCheck ignoreExpansionCheckingFailure allConstructions allResolvedConstructions
  putStrLn "\nParsing source"
  node <- ambiguityParse langConstructions startSym source
  putStrLn "\nResolving source"
  node <- resolveSource allResolvedConstructions node
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

benchOperations :: IO ()
benchOperations = do
  [startSym, coreGrammar, grammar, fragmentFile] <- filter (not . isOption) <$> getArgs
  fragment <- readFile fragmentFile >>= evaluate . force
  let testPrograms = iterate (fragment ++) fragment
  benchmark startSym coreGrammar grammar testPrograms

grammar :: (_, (_, _)) -> FilePath -> IO (_, (_, _))
grammar (constrs, (_, resConstrs)) path = do
  putStrLn $ "Parsing " ++ path
  constructions <- getConstructions (implementationGrammar constrs) path
  putStrLn $ "Resolving " ++ path
  resolvedConstructions <- resolveConstructions constructions
  return (M.union constrs constructions, (constructions, M.union resConstrs resolvedConstructions))

resolveSource :: Gen pre => M.Map String ResolvedConstruction -> FixNode NoSplice pre -> IO (FixNode NoSplice GenSym)
resolveSource resolvedConstructions node = case resolveNames resolvedConstructions node of
  Data res -> return res
  Error es -> do
    putStrLn . prettyShow $ node
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
    parseFile parser "-" = getContents >>= evaluate . parser . tokenize . force
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

implCheck :: Bool -> M.Map String (Constr _) -> M.Map String ResolvedConstruction -> IO ()
implCheck ignoreFailure constructions resolvedConstructions = if S.null errors
  then return ()
  else do
    forM_ errors $ putStrLn . show
    if ignoreFailure
      then putStrLn $ "WARNING: continuing despite expansion check failure"
      else error $ "Could continue, but will not (use -iec or --ignore-expansion-check)"
  where
    errors = check constructions resolvedConstructions

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

benchmark :: String -> FilePath -> FilePath -> [String] -> IO ()
benchmark startSym corePath langPath sources = do
  coreConstructions <- getConstructions noImpl corePath
  resolvedCoreConstructions <- resolveConstructions coreConstructions
  constructions <- getConstructions (implementationGrammar coreConstructions) langPath
  resolvedConstructions <- resolveConstructions constructions
  let allResolvedConstructions = M.union resolvedConstructions resolvedCoreConstructions
  forM_ (zip [1..] sources) $ \(idx, source) -> do
    evaluate $ force source
    benchRow (show idx) $ do
      tokens <- benchAction $ tokenize source
      [node] <- benchAction . fst $ parseWithGrammar startSym constructions tokens
      node' <- benchAction . fromData $ resolveNames resolvedConstructions node
      expanded <- benchAction $ fullExpansion allResolvedConstructions node'
      void . benchAction $ removeEFuns expanded
  where
    fromData (Data a) = a

benchRow :: String -> IO a -> IO ()
benchRow rowLabel m = do
  putStr $ "\n" ++ rowLabel
  void m
  hFlush stdout

benchAction :: NFData a => a -> IO a
benchAction ret = do
  startTime <- getCurrentTime
  _ <- evaluate $ force ret
  endTime <- getCurrentTime
  _ <- evaluate $ force ret
  postForce <- getCurrentTime
  let full = postForce `diffUTCTime` startTime
      forceOnly = postForce `diffUTCTime` endTime
      firstOnly = endTime `diffUTCTime` startTime
  -- putStr $ ", " ++ show full
  --       ++ ", " ++ show (firstOnly - forceOnly)
  --       ++ ", " ++ show forceOnly
  putStr $ ", " ++ show ((realToFrac $ firstOnly - forceOnly) :: Double)
  return ret

  -- TODO: it is annoying to give header everytime, and engineering formatting might be annoying for google sheets, if that is what I'll use

unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr go Nothing
  where
    go x mxs = Just (case mxs of
       Nothing -> ([], x)
       Just (xs, e) -> (x:xs, e))
