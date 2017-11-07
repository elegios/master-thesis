{-# LANGUAGE RecursiveDo #-}

module Binding (resolve, Result(..)) where

import Control.Arrow ((&&&), (***))
import Control.Applicative ((<|>))
import Data.Foldable (sequenceA_)
import Data.Traversable (forM)
import Data.Maybe (fromMaybe)
import Data.Function ((&))

import qualified Data.Map.Lazy as M
import qualified Data.Set as S

import Control.Monad.Tardis (Tardis, evalTardis, getsPast, getFuture, modifyForwards, modifyBackwards, sendPast)

import Types.Lexer (Range)
import Types.Construction (Construction(extraData), ExtraData(ExtraData, bindingData, beforeBindings, afterBindings))
import Types.Ast

{-
Assumptions:
Each identifier only appears once on the right side of a binding (i.e. not in multiple bindings), so that scopes are strictly nested
Currently disallows multiple definitions of the same name in the same scope (even if the definitions aren't overlapping due to parent_up then parent_down)
Much is assumed about the binding spec and its well-formedness
no binding stuff refers to non-top level names (not sure what the semantics of that should be)
currently assumes that everything on the left of a binding is a singular identifier, while it should be allowable to have a repeated identifier
-}

data GenSym = GenSym String Int

instance Show GenSym where
  show (GenSym s i) = s ++ "#" ++ show i

type LookupTable = M.Map String GenSym

data ResolverState = ResolverState
  { nextSym :: Int
  , extra :: M.Map String ExtraData
  , lookupTable :: LookupTable
  , definedInThisScope :: M.Map String Range }

type Resolver a = Tardis LookupTable ResolverState a

data Result e a = Data a | Error [e]

type Res = Result String

resolve :: M.Map String (Construction n) -> NodeI String -> Result String (NodeI GenSym)
resolve constructions n = evalTardis (res n) (M.empty, initial)
  where
    initial = ResolverState
      { nextSym = 0
      , extra = extraData <$> constructions
      , lookupTable = M.empty
      , definedInThisScope = M.empty }

class Resolvable r where
  res :: r String -> Resolver (Res (r GenSym))

instance Resolvable NodeI where
  res n@Node{name, children} = fmap (\cs -> n { children = cs }) <$> do
    ExtraData{beforeBindings, afterBindings, bindingData} <- getsPast $ (M.! name) . extra
    let mChildren = M.fromList children
        getId n = case mChildren M.! n of MidIdentifier _ i -> i
        defining = S.fromList . concat . (beforeBindings:) . (afterBindings:) $ fst <$> bindingData
        unscoped = M.withoutKeys mChildren . S.union defining . S.fromList . concat $ snd <$> bindingData
    syms <- M.fromList <$> mapM (\n -> (n,) <$> gensym (getId n)) (S.toList defining)
    let defineName def n = let MidIdentifier r i = mChildren M.! n
                               sym = syms M.! n
                           in def i r sym
        identifierResolve i = case mChildren M.! i of
          MidIdentifier r _ -> MidIdentifier r $ syms M.! i
        definingChildren = M.fromSet (pure . identifierResolve) defining
    parentUpDefs <- mapM (defineName definePast) $ beforeBindings
    (scopeDefs, scopedChildren) <- fmap ((concat *** M.unions) . unzip) . forM bindingData $ \(binders, bodies) -> scope $ do
      scopeDefs <- mapM (defineName defineFuture) binders
      bodies <- fmap M.fromList . forM bodies $ \b ->
        (b,) <$> res (mChildren M.! b)
      return (scopeDefs, bodies)
    unscopedChildren <- mapM res unscoped
    parentDownDefs <- mapM (defineName defineFuture) $ afterBindings
    let allChildren = M.unions [unscopedChildren, scopedChildren, definingChildren]
        children' = (\s -> (s,) <$> allChildren M.! s) . fst <$> children
    return $ sequenceA_ parentUpDefs *>
             sequenceA_ parentDownDefs *>
             sequenceA_ scopeDefs *>
             sequenceA children'

instance Resolvable MidNodeI where
  res (MidNode n) = fmap MidNode <$> res n
  res (MidIdentifier r i) = fmap (MidIdentifier r) <$> lookupBinding i r
  res (SyntaxSplice r i) = return . pure $ SyntaxSplice r i
  res (Basic t) = return . pure $ Basic t
  res (Repeated rep cs) = fmap (Repeated rep) . sequenceA
    <$> mapM res cs
  res (Sequenced r cs) = fmap (Sequenced r) . sequenceA
    <$> mapM (\(s, n) -> fmap (s,) <$> res n) cs

resError :: e -> Result e a
resError e = Error [e]

scope :: Resolver a -> Resolver a
scope r = mdo
  (beforeTable, beforeDefined) <- getsPast $ lookupTable &&& definedInThisScope
  modifyForwards (\r -> r {definedInThisScope = M.empty})
  sendPast afterTable
  res <- r
  afterTable <- getFuture
  modifyForwards (\r -> r { lookupTable = beforeTable
                          , definedInThisScope = beforeDefined })
  return res

defineFuture :: String -> Range -> GenSym -> Resolver (Res ())
defineFuture s r sym = do
  prevDef <- M.lookup s <$> getsPast definedInThisScope
  case prevDef of
    Just r' -> return . resError $ s ++ " at " ++ show r ++ " is already defined in this scope (at " ++ show r' ++ ")"
    Nothing -> do
      modifyForwards $ \res@ResolverState{lookupTable, definedInThisScope} ->
        res { lookupTable = M.insert s sym lookupTable
            , definedInThisScope = M.insert s r definedInThisScope }
      return $ Data ()

definePast :: String -> Range -> GenSym -> Resolver (Res ())
definePast s r sym = do
  prevDef <- M.lookup s <$> getsPast definedInThisScope
  case prevDef of
    Just r' -> return . resError $ s ++ " at " ++ show r ++ " is already defined in this scope (at " ++ show r' ++ ")"
    Nothing -> do
      modifyForwards $ \res@ResolverState{definedInThisScope} ->
        res { definedInThisScope = M.insert s r definedInThisScope}
      modifyBackwards $ M.insert s sym
      return $ Data ()

lookupBinding :: String -> Range -> Resolver (Res GenSym)
lookupBinding s r = do
  mPast <- M.lookup s <$> getsPast lookupTable
  mFuture <- M.lookup s <$> getFuture
  (mPast <|> mFuture)
    & fmap Data
    & fromMaybe (resError $ "Undefined variable " ++ s ++ " at " ++ show r)
    & return

gensym :: String -> Resolver GenSym
gensym s = GenSym s <$> getsPast nextSym
  <* modifyForwards (\r@ResolverState{nextSym} -> r {nextSym = nextSym + 1})

instance Functor (Result e) where
  fmap f (Data a) = Data $ f a
  fmap f (Error es) = Error es

instance Applicative (Result e) where
  pure = Data
  (Data f) <*> (Data a) = Data $ f a
  (Error e1) <*> (Error e2) = Error $ mappend e1 e2
  (Error es) <*> _ = Error es
  _ <*> (Error es) = Error es
