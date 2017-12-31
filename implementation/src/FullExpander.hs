module FullExpander where

import Debug.Trace

import Control.Applicative ((<|>))

import qualified Data.Map as M

import Binding

import Types.Result
import Types.Construction(NoSplice)
import Types.ResolvedConstruction
import Types.GenSym
import Types.Ast

type Node = FixNode NoSplice GenSym
type MidNode = MidNodeI (NoSplice Node) GenSym

fullExpansion :: M.Map String ResolvedConstruction -> Node -> Node
fullExpansion constructions = recur
  where
    recur prev@(FixNode n) = case expandOne expander $ MidNode n of
      Nothing -> prev
      Just (MidNode expanded) -> case resolveNames constructions $ FixNode expanded of
        Data resolved -> recur resolved
        Error es -> error $ "Name resolution error(s) during expansion: " ++ show es ++ "\ntree:\n" ++ prettyShow (FixNode expanded)
      Just m -> error $ "Compiler error: Somehow expanded to a midnode: " ++ show m
    expander name n = (M.lookup name constructions <|> err name >>= expand) <*> pure n
    err name = error $ "Compiler error: Unknown construction: " ++ show name ++ ", constructions keys: " ++ show (M.keys constructions)

-- Gives Nothing if there is nothing to expand, i.e. everything left is builtin
expandOne :: (String -> Node -> Maybe MidNode) -> MidNode -> Maybe MidNode
expandOne expander = \case
  MidNode n@Node{name, children} -> (\ms -> MidNode $ n {children = ms}) <$> expandChild children
                                <|> trace name <$> expander name (FixNode n)
  Repeated rep ms -> Repeated rep <$> expandChild ms
  Sequenced r ms -> Sequenced r <$> expandChild ms
  _ -> Nothing
  where
    expandChild :: [MidNode] -> Maybe [MidNode]
    expandChild [] = Nothing
    expandChild (m:ms) = (:ms) <$> expandOne expander m <|> (m:) <$> expandChild ms
