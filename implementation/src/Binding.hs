{-# LANGUAGE RecursiveDo, ViewPatterns, FlexibleContexts #-}

module Binding (resolveNames, Error(..), Gen) where

import Prelude hiding (lookup)
import Data.List (unzip4, mapAccumL)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.Foldable (asum, find, sequenceA_)
import Control.Arrow (first, second)
import Control.Monad ((>=>))
import Control.Applicative ((<|>))

import qualified Data.Map as M

import Control.Monad.Tardis (Tardis, evalTardis, modifyForwards, modifyBackwards, getsPast, getPast, getFuture, sendPast)

import Types.Lexer (Range)
import Types.Construction (NoSplice)
import Types.Ast
import Types.Paths
import Types.ResolvedConstruction
import Types.Result
import Types.GenSym

-- TODO: #bind x in body, scope(x) must be reachable from scope(body)

type Node i = NodeI (NoSplice (FixNode NoSplice i)) i
type MidNode i = MidNodeI (NoSplice (FixNode NoSplice i)) i

data ScopeInstance = ScopeInstance Scope [Int] deriving (Eq, Ord, Show)

leaves :: Node pre -> [(SinglePath, MidNode pre)]
leaves Node{children} = multiMid children
  where
    multiMid = zip [0..] >=> \(i, m) -> first (prepend i) <$> midLeaves m
    midLeaves m@MidNode{} = [(here, m)]
    midLeaves m@MidIdentifier{} = [(here, m)]
    midLeaves MidSplice{} = error $ "Compiler error: Unexpected splice in midLeaves"
    midLeaves m@Basic{} = [(here, m)]
    midLeaves (Repeated _ ms) = multiMid ms
    midLeaves (Sequenced _ ms) = multiMid ms
leaves SyntaxSplice{} = error $ "Compiler error: Unexpected splice in leaves"

replaceLeaves :: (Show pre, Show post) => Node pre -> [MidNode post] -> Node post
replaceLeaves n@Node{children} ms' = n { children = snd $ mapAccumL midReplace ms' children }
  where
    midReplace (m'@MidNode{} : ms') MidNode{} = (ms', m')
    midReplace (m'@MidIdentifier{} : ms') MidIdentifier{} = (ms', m')
    midReplace _ MidSplice{} = error $ "Compiler error: Unexpected splice in midReplace"
    midReplace (m'@Basic{} : ms') Basic{} = (ms', m')
    midReplace ms' (Repeated rep ms) = Repeated rep `second` mapAccumL midReplace ms' ms
    midReplace ms' (Sequenced r ms) = Sequenced r `second` mapAccumL midReplace ms' ms
    midReplace (m':_) m = error $ "Compiler error: mismatching leaves: m = " ++ show m ++ ", m' = " ++ show m'
    midReplace [] m = error $ "Compiler error: ran out of replacement leaves, m = " ++ show m
replaceLeaves SyntaxSplice{} _ = error $ "Compiler error: Unexpected splice in replaceLeaves"

data Error = RedefError String Range Range
           | UndefError String Range
           deriving (Show) -- TODO: probably use a nicer instance for Show, or some other typeclass

type HorizontalBindings pre = M.Map ScopeInstance (M.Map pre (Range, GenSym))
type VerticalBindings pre = M.Map ScopeInstance (M.Map pre [(TreePath, (Range, GenSym))])

data ResolverState pre = ResolverState
  { nextSym :: Int
  , constructions :: M.Map String ResolvedConstruction
  , construction :: ResolvedConstruction
  , afterBindingsR :: HorizontalBindings pre
  , inBindingsR :: VerticalBindings pre
  , gensyms :: M.Map SinglePath GenSym }

type Resolver pre a = Tardis (HorizontalBindings pre) (ResolverState pre) a

resolveNames :: Gen pre => M.Map String ResolvedConstruction -> FixNode NoSplice pre -> Result (FixNode NoSplice GenSym)
resolveNames constructions (FixNode n) = FixNode <$> evalTardis (resolve n) (bw, fw)
  where
    bw = M.empty
    fw = ResolverState { nextSym = 0
                       , constructions = constructions
                       , construction = error "Compiler error: looked for a construction outside of one" -- This should be ok
                       , afterBindingsR = M.empty
                       , inBindingsR = M.empty
                       , gensyms = M.empty }

-- TODO: check for overlap between vertical and horizontal at the edges
resolve :: Gen pre => Node pre -> Resolver pre (Result (Node GenSym))
resolve SyntaxSplice{} = error $ "Compiler error: unexpected splice in resolve"
resolve node@Node{name} = do
  rc@ResolvedConstruction{beforeBindings, afterBindings, inBindings} <- getsPast $ (M.! name) . constructions
  modifyForwards $ \rs -> rs { construction = rc, gensyms = M.empty }
  let ops leaf = ( getExport beforeBindings leaf
                 , getExport afterBindings leaf
                 , getVerticalExport inBindings leaf
                 , midRes leaf )
      (before, after, vertical, resolveMids) = unzip4 $ ops <$> leaves node
      horiSymbols = (\(p, _, symbol) -> (p, symbol)) <$> catMaybes (before ++ after)
      vertSymbols = (\(_, (p, _, symbol)) -> (p, symbol)) <$> catMaybes vertical
  mapM_ gensym $ horiSymbols ++ vertSymbols
  resIn <- fmap sequenceA_ . mapM defineVertical $ catMaybes vertical
  resBefore <- fmap sequenceA_ . mapM defineBefore $ catMaybes before
  resLeaves <- fmap sequenceA $ sequence resolveMids
  resAfter <- fmap sequenceA_ . mapM defineAfter $ catMaybes after
  return $ resBefore *> resIn *> (replaceLeaves node <$> resLeaves) <* resAfter

getExport :: TreeEndPath -> (SinglePath, MidNode pre) -> Maybe (SinglePath, Range, pre)
getExport defPath (p, MidIdentifier r symbol)
  | p `oneOf` defPath = Just (p, r, symbol)
getExport _ _ = Nothing

getVerticalExport :: [(TreeEndPath, TreePath)] -> (SinglePath, MidNode pre) -> Maybe (TreePath, (SinglePath, Range, pre))
getVerticalExport verticals (p, MidIdentifier r symbol) = case find (oneOf p . fst) verticals of
  Just (_, inPath) -> Just (inPath, (p, r, symbol))
  Nothing -> Nothing
getVerticalExport _ _ = Nothing

-- TODO: when midRes has resolved a Node, examine the new horizontal bindings for overlaps with vertical bindings
midRes :: Gen pre => (SinglePath, MidNode pre) -> Resolver pre (Result (MidNode GenSym))
midRes (p, MidNode n) = mdo
  ResolverState{construction, gensyms} <- getPast
  sc <- getScope p
  resN <- flattenBackwards sc . flattenForwards sc $ resolve n
  modifyForwards $ \rs ->
    rs { construction = construction, gensyms = gensyms }
  return $ MidNode <$> resN
    where
      flattenForwards sc res = do
        ResolverState{afterBindingsR=prevAfterBindings, inBindingsR} <- getPast
        let singleVertLookup sc = M.mapMaybe findVertical . fromMaybe M.empty $ M.lookup sc inBindingsR
            findVertical = listToMaybe . map snd . filter (childOf p . fst)
            singleScLookup sc = singleVertLookup sc `M.union` singleHoriLookup prevAfterBindings sc
            (closeAfter, farAfter) = case singleScLookup <$> scopeChain sc of
              close : far -> (close, M.unions far)
              _ -> error $ "Compiler error: scopeChain gave no results"
        modifyForwards $ \rs ->
          rs { afterBindingsR = M.fromList [(closeSc, closeAfter), (farSc, farAfter)]
             , inBindingsR = M.empty}
        result <- res
        newCloseAfter <- getsPast $ (`M.difference` closeAfter) . fromMaybe M.empty . M.lookup closeSc . afterBindingsR
        -- TODO: check newCloseAfter overlap with inBindingsR
        modifyForwards $ \rs ->
          rs { afterBindingsR = M.union newCloseAfter `alter` sc $ prevAfterBindings
             , inBindingsR = inBindingsR }
        return result
      flattenBackwards sc res = mdo
        sendPast $ M.union newCloseBefore `alter` sc $ prevBeforeBindings
        -- TODO: check newCloseBefore overlap with inBindings
        newCloseBefore <- (`M.difference` closeBefore) . fromMaybe M.empty . M.lookup closeSc <$> getFuture
        result <- res
        sendPast $ M.fromList [(closeSc, closeBefore), (farSc, farBefore)]
        let (closeBefore, farBefore) =
              case singleHoriLookup prevBeforeBindings <$> scopeChain sc of
                close : far -> (close, M.unions far)
                _ -> error $ "Compiler error: scopeChain gave no results"
        prevBeforeBindings <- getFuture
        return result
      singleHoriLookup bindings sc = fromMaybe M.empty $ M.lookup sc bindings

midRes (p, MidIdentifier r symbol) = fmap (fmap $ MidIdentifier r) $
  getsPast (M.lookup p . gensyms) >>= maybe (lookup (r, symbol) p) (return . pure)
midRes (_, MidSplice{}) = error $ "Compiler error: encountered splice in name resolution"
midRes (_, Basic t) = return . pure $ Basic t
midRes (_, Repeated{}) = error $ "Compiler error: Repeated is not a leaf"
midRes (_, Sequenced{}) = error $ "Compiler error: Sequenced is not a leaf"

getScope :: SinglePath -> Resolver pre ScopeInstance
getScope p = do
  scs <- getsPast (scopes . construction)
  return . makeInstance . fromMaybe CloseScope $ find pIsIn scs
  where
    pIsIn (Scope _ elementsPath _) = p `childOf` elementsPath
    pIsIn _ = False
    makeInstance sc@(Scope repeatPath _ _) = ScopeInstance sc $ calculatePathInstance repeatPath p
    makeInstance sc = ScopeInstance sc []

{-
Note the asymmetry between defineBefore and defineAfter, the former
checks for overlap with both before and after bindings, while
the latter only checks after bindings. This is to only report any
single error once.
-}
defineBefore :: Gen pre => (SinglePath, Range, pre) -> Resolver pre (Result ())
defineBefore (p, r, symbol) = do
  sym <- getGenSym p
  bindingsP <- getsPast afterBindingsR
  modifyBackwards $ multiInsert closeSc symbol (r, sym)
  bindingsF <- getFuture
  return . maybe ok mkError $ multiLookup closeSc symbol bindingsP
                          <|> multiLookup closeSc symbol bindingsF
  where
    mkError (r', _) = redefError symbol r r'

defineAfter :: Gen pre => (SinglePath, Range, pre) -> Resolver pre (Result ())
defineAfter (p, r, symbol) = do
  sym <- getGenSym p
  bindingsP <- getsPast afterBindingsR
  modifyForwards $ \rs@ResolverState{afterBindingsR} ->
    rs { afterBindingsR = multiInsert closeSc symbol (r, sym) afterBindingsR }
  return . maybe ok mkError $ multiLookup closeSc symbol bindingsP
  where
    mkError (r', _) = redefError symbol r r'

closeSc :: ScopeInstance
closeSc = ScopeInstance CloseScope []

farSc :: ScopeInstance
farSc = ScopeInstance FarScope []

{-
Notice that defineVertical makes no overlap check with horizontal bindings,
nor does defineBefore/defineAfter check for overlap with vertical
bindings. The checks are at beginning and end of resolve, and in
midRes before and after calling resolve.
-- TODO: check with previous after bindings, since that's how outside bindings will most likely enter through resolve
-- TODO: the above won't work I think, I think it won't be lazy enough
-}
defineVertical :: Gen pre => (TreePath, (SinglePath, Range, pre)) -> Resolver pre (Result ())
defineVertical (tp, (p, r, symbol)) = do
  sc <- getScope p
  sym <- getGenSym p
  prevVerticals <- getsPast $ concat . multiLookup sc symbol . inBindingsR
  modifyForwards $ \rs@ResolverState{inBindingsR} ->
    rs { inBindingsR = insert sc (tp, (r, sym)) inBindingsR }
  return . maybe ok mkError $ find (childrenOverlapping tp . fst) prevVerticals
  where
    mkError (_, (r', _)) = redefError symbol r r'
    insert sc a = ((a:) `alter` symbol) `alter` sc

lookup :: Gen pre => (Range, pre) -> SinglePath -> Resolver pre (Result GenSym)
lookup (r, symbol) p = do
  sc <- getScope p
  fmap (maybe err pure . asum) . mapM singleScLookup $ scopeChain sc
  where
    err = undefError symbol r
    singleScLookup sc = do
      mAfter <- singleHoriLookup sc <$> getsPast afterBindingsR
      mBefore <- singleHoriLookup sc <$> getFuture
      mIn <- singleVertLookup sc <$> getsPast inBindingsR
      return $ mIn <|> mAfter <|> mBefore
    singleHoriLookup sc bindings = snd <$> (M.lookup sc bindings >>= M.lookup symbol)
    singleVertLookup sc bindings = listToMaybe . fmap (snd . snd) . filter (childOf p . fst) . concat $ multiLookup sc symbol bindings

scopeChain :: ScopeInstance -> [ScopeInstance]
scopeChain sc@(ScopeInstance FarScope _) = [sc]
scopeChain sc@(ScopeInstance CloseScope _) = [sc, ScopeInstance FarScope []]
scopeChain sc@(ScopeInstance (Scope _ _ sc') inst) = sc : scopeChain (ScopeInstance sc' $ limitInstance sc' inst)
  where
    limitInstance (Scope mp _ _) inst = limitPathInstance mp inst
    limitInstance _ _ = []

getGenSym :: SinglePath -> Resolver pre GenSym
getGenSym p = getsPast $ fromMaybe err . M.lookup p . gensyms
  where
    err = error $ "Compiler error: path " ++ show p ++ " should have a gensym already, but doesn't"

-- Gen s means that s can be used as a pre symbol in name resolution
class (Ord pre, Show pre) => Gen pre where
  getString :: pre -> String
  gensym :: (SinglePath, pre) -> Resolver pre GenSym
  gensym (p, symbol) = mkSym <* advanceSym
    where
      advanceSym = modifyForwards (\r@ResolverState{nextSym} -> r {nextSym = nextSym + 1})
      mkSym = do
        sym <- GenSym (getString symbol) <$> getsPast nextSym
        modifyForwards $ \rs@ResolverState{gensyms} -> rs { gensyms = M.insert p sym gensyms }
        return sym

instance Gen String where
  getString = id
instance Gen GenSym where
  getString (GenSym s _) = s

-- helpers

multiInsert :: (Ord k1, Ord k2) => k1 -> k2 -> a -> M.Map k1 (M.Map k2 a) -> M.Map k1 (M.Map k2 a)
multiInsert k1 k2 a = M.insert k2 a `alter` k1

multiLookup :: (Ord k1, Ord k2) => k1 -> k2 -> M.Map k1 (M.Map k2 a) -> Maybe a
multiLookup k1 k2 = M.lookup k1 >=> M.lookup k2

alter :: (Ord k, Monoid a) => (a -> a) -> k -> M.Map k a -> M.Map k a
alter f = M.alter $ Just . f . fromMaybe mempty

redefError :: Gen pre => pre -> Range -> Range -> Result a
redefError symbol r r' = Error . pure $ RedefError (getString symbol) (min r r') (max r r')

undefError :: Gen pre => pre -> Range -> Result a
undefError symbol r = Error . pure $ UndefError (getString symbol) r

type Result a = ResultT [Error] a
