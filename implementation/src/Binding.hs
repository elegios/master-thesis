{-# LANGUAGE RecursiveDo, ViewPatterns, FlexibleContexts #-}

module Binding (resolveNames, Error(..), Gen ,resolveNamesS, SpliceBindings(..)) where

import Prelude hiding (lookup)
import Data.List (unzip4, mapAccumL)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.Foldable (asum, find, sequenceA_)
import Data.Function ((&))
import Data.Monoid ((<>))
import Control.Arrow (first, second)
import Control.Monad ((>=>))
import Control.Applicative ((<|>))

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Tardis (Tardis, runTardis, modifyForwards, modifyBackwards, getsPast, getPast, getsFuture, getFuture, sendPast)

import Types.Lexer (Range)
import Types.Construction (NoSplice)
import Types.Ast
import Types.Paths
import Types.ResolvedConstruction
import Types.Result
import Types.GenSym

-- TODO: #bind x in body, scope(x) must be reachable from scope(body)

type Node s i = NodeI (s (FixNode s i)) i
type MidNode s i = MidNodeI (s (FixNode s i)) i

data ScopeInstance = ScopeInstance Scope [Int] deriving (Eq, Ord, Show)

leaves :: Node s pre -> [(SinglePath, MidNode s pre)]
leaves Node{children} = multiMid children
  where
    multiMid = zip [0..] >=> \(i, m) -> first (prepend i) <$> midLeaves m
    midLeaves m@MidNode{} = [(here, m)]
    midLeaves m@MidIdentifier{} = [(here, m)]
    midLeaves m@MidSplice{} = [(here, m)]
    midLeaves m@Basic{} = [(here, m)]
    midLeaves (Repeated _ ms) = multiMid ms
    midLeaves (Sequenced _ ms) = multiMid ms
leaves (SyntaxSplice s) = [(here, MidSplice s)]

replaceLeaves :: (Show pre, Show post, Show (s (FixNode s pre)), Show (s (FixNode s post))) => Node s pre -> [MidNode s post] -> Node s post
replaceLeaves n@Node{children} ms' = n { children = snd $ mapAccumL midReplace ms' children }
  where
    midReplace (m'@MidNode{} : ms') MidNode{} = (ms', m')
    midReplace (m'@MidIdentifier{} : ms') MidIdentifier{} = (ms', m')
    midReplace (m'@MidSplice{} : ms') MidSplice{} = (ms', m')
    midReplace (m'@Basic{} : ms') Basic{} = (ms', m')
    midReplace ms' (Repeated rep ms) = Repeated rep `second` mapAccumL midReplace ms' ms
    midReplace ms' (Sequenced r ms) = Sequenced r `second` mapAccumL midReplace ms' ms
    midReplace (m':_) m = error $ "Compiler error: mismatching leaves: m = " ++ show m ++ ", m' = " ++ show m'
    midReplace [] m = error $ "Compiler error: ran out of replacement leaves, m = " ++ show m
replaceLeaves SyntaxSplice{} [MidSplice s] = SyntaxSplice s
replaceLeaves n ms' = error $ "Compiler error: mismatched leaf replacement, n: " ++ show n ++ " ms': " ++ show ms'

data Error = RedefError String Range Range
           | UndefError String Range
           deriving (Show, Eq, Ord) -- TODO: probably use a nicer instance for Show, or some other typeclass

type HorizontalBindings pre = M.Map ScopeInstance (M.Map pre (Range, GenSym))
type VerticalBindings pre = M.Map ScopeInstance (M.Map pre [(TreePath, (Range, GenSym))])

data ResolverState spliceBinding spliceResult s pre = ResolverState
  { nextSym :: Int
  , constructions :: M.Map String ResolvedConstruction
  , construction :: ResolvedConstruction
  , afterBindingsR :: HorizontalBindings pre
  , inBindingsR :: VerticalBindings pre
  , gensyms :: M.Map SinglePath GenSym
  , resSplice :: S.Set pre -> (spliceBinding, spliceBinding) -> s (FixNode s pre) -> ((spliceBinding, spliceBinding), (s (FixNode s GenSym), spliceResult))
  , afterSpliceR :: M.Map ScopeInstance spliceBinding
  , spliceResult :: spliceResult }

data ReverseResolverState spliceBinding pre = ReverseResolverState
  { beforeBindingsR :: HorizontalBindings pre
  , beforeSpliceR :: M.Map ScopeInstance spliceBinding }

type Resolver spliceBinding spliceResult s pre a =
  Tardis (ReverseResolverState spliceBinding pre)
         (ResolverState spliceBinding spliceResult s pre)
         a

resolveNames :: Gen pre => M.Map String ResolvedConstruction -> FixNode NoSplice pre -> Result (FixNode NoSplice GenSym)
resolveNames constructions n = fst $ resolveNamesS resSplice constructions n
  where
    resSplice :: S.Set pre -> ((), ()) -> NoSplice (FixNode NoSplice pre) -> (((), ()), (NoSplice (FixNode NoSplice GenSym), ()))
    resSplice = error $ "Compiler error: encountered a splice during name resolution"

data SpliceBindings spliceResult spliceBinding pre = SpliceBindings
  { result :: spliceResult
  , beforeSplice :: spliceBinding
  , afterSplice :: spliceBinding
  , beforeBinding :: S.Set pre
  , afterBinding :: S.Set pre }

resolveNamesS :: (Gen pre, Show (s (FixNode s pre)), Show (s (FixNode s GenSym)), Monoid spliceBinding, Monoid spliceResult)
  => (S.Set pre -> (spliceBinding, spliceBinding) -> s (FixNode s pre) -> ((spliceBinding, spliceBinding), (s (FixNode s GenSym), spliceResult)))
  -> M.Map String ResolvedConstruction
  -> FixNode s pre
  -> (Result (FixNode s GenSym), SpliceBindings spliceResult spliceBinding pre)
resolveNamesS resSplice constructions (FixNode n) =
  let (n', ( ReverseResolverState{beforeBindingsR, beforeSpliceR}
           , ResolverState{spliceResult, afterBindingsR, afterSpliceR})) = runTardis (resolve n) (bw, fw)
      spliceBindings = SpliceBindings
        { result = spliceResult
        , beforeSplice = getClose beforeSpliceR
        , afterSplice = getClose afterSpliceR
        , beforeBinding = M.keysSet $ getClose beforeBindingsR
        , afterBinding = M.keysSet $ getClose afterBindingsR }
  in (FixNode <$> n', spliceBindings)
  where
    bw = ReverseResolverState { beforeBindingsR = M.empty
                              , beforeSpliceR = M.empty }
    fw = ResolverState { nextSym = 0
                       , constructions = constructions
                       , construction = error "Compiler error: looked for a construction outside of one" -- This should be ok
                       , afterBindingsR = M.empty
                       , inBindingsR = M.empty
                       , gensyms = M.empty
                       , resSplice = resSplice
                       , afterSpliceR = M.empty
                       , spliceResult = mempty }

-- TODO: check for overlap between vertical and horizontal at the edges
resolve :: (Gen pre, Monoid spliceBinding, Monoid spliceResult, Show (s (FixNode s pre)), Show (s (FixNode s GenSym)))
        => Node s pre -> Resolver spliceBinding spliceResult s pre (Result (Node s GenSym))
resolve (SyntaxSplice s) = pure . SyntaxSplice <$> resolveSplice here s
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

getExport :: TreeEndPath -> (SinglePath, MidNode s pre) -> Maybe (SinglePath, Range, pre)
getExport defPath (p, MidIdentifier r symbol)
  | p `oneOf` defPath = Just (p, r, symbol)
getExport _ _ = Nothing

getVerticalExport :: [(TreeEndPath, TreePath)] -> (SinglePath, MidNode s pre) -> Maybe (TreePath, (SinglePath, Range, pre))
getVerticalExport verticals (p, MidIdentifier r symbol) = case find (oneOf p . fst) verticals of
  Just (_, inPath) -> Just (inPath, (p, r, symbol))
  Nothing -> Nothing
getVerticalExport _ _ = Nothing

-- TODO: when midRes has resolved a Node, examine the new horizontal bindings for overlaps with vertical bindings
midRes :: (Gen pre, Monoid spliceResult, Monoid spliceBinding, Show (s (FixNode s pre)), Show (s (FixNode s GenSym)))
       => (SinglePath, MidNode s pre) -> Resolver spliceBinding spliceResult s pre (Result (MidNode s GenSym))
midRes (p, MidNode n) = mdo
  ResolverState{construction, gensyms} <- getPast
  sc <- getScope p
  resN <- flattenBackwards sc . flattenForwards sc $ resolve n
  modifyForwards $ \rs ->
    rs { construction = construction, gensyms = gensyms }
  return $ MidNode <$> resN
    where
      flattenForwards sc res = do
        ResolverState{ afterBindingsR = prevAfterBindings
                     , afterSpliceR = prevAfterSplice
                     , inBindingsR} <- getPast
        let singleVertLookup sc = M.mapMaybe findVertical . fromMaybe M.empty $ M.lookup sc inBindingsR
            findVertical = listToMaybe . map snd . filter (childOf p . fst)
            singleScLookup sc = singleVertLookup sc `M.union` singleHoriLookup prevAfterBindings sc
            (closeAfter, farAfter) = case singleScLookup <$> scopeChain sc of
              close : far -> (close, M.unions far)
              _ -> error $ "Compiler error: scopeChain gave no results"
            (closeAfterSplice, farAfterSplice) = case singleHoriLookup prevAfterSplice <$> scopeChain sc of
              close : far -> (close, mconcat far)
              _ -> error $ "Compiler error: scopeChain gave no results"
        modifyForwards $ \rs ->
          rs { afterBindingsR = M.fromList [(closeSc, closeAfter), (farSc, farAfter)]
             , afterSpliceR = M.fromList [(closeSc, closeAfterSplice), (farSc, farAfterSplice)]
             , inBindingsR = M.empty}
        result <- res
        ResolverState{ afterBindingsR = getClose -> nextCloseAfter
                     , afterSpliceR = getClose -> nextCloseAfterSplice } <- getPast
        let newCloseAfter = nextCloseAfter `M.difference` closeAfter
        -- TODO: check newCloseAfter overlap with inBindingsR
        -- NOTE: when setting afterBindingsR below we use M.union of the new things since closeAfter will contain some inBindings too
        modifyForwards $ \rs ->
          rs { afterBindingsR = M.union newCloseAfter `alter` sc $ prevAfterBindings
             , afterSpliceR = M.insert sc nextCloseAfterSplice prevAfterSplice
             , inBindingsR = inBindingsR }
        return result
      flattenBackwards sc res = mdo
        sendPast $ ReverseResolverState
          { beforeBindingsR = M.insert sc nextCloseBefore prevBeforeBindings
          , beforeSpliceR = M.insert sc nextCloseBeforeSplice prevBeforeSplice }
        -- TODO: check newCloseBefore overlap with inBindings
        -- let newCloseBefore = nextCloseBefore `M.difference` closeBefore
        ~ReverseResolverState{ beforeBindingsR = getClose -> nextCloseBefore
                             , beforeSpliceR = getClose -> nextCloseBeforeSplice } <- getFuture
        result <- res
        sendPast $ ReverseResolverState
          { beforeBindingsR = M.fromList [(closeSc, closeBefore), (farSc, farBefore)]
          , beforeSpliceR = M.fromList [(closeSc, closeBeforeSplice), (farSc, farBeforeSplice)] }
        let (closeBefore, farBefore) =
              case singleHoriLookup prevBeforeBindings <$> scopeChain sc of
                close : far -> (close, M.unions far)
                _ -> error $ "Compiler error: scopeChain gave no results"
            (closeBeforeSplice, farBeforeSplice) =
              case singleHoriLookup prevBeforeSplice <$> scopeChain sc of
                close : far -> (close, mconcat far)
                _ -> error $ "Compiler error: scopeChain gave no results"
        ~ReverseResolverState{ beforeBindingsR = prevBeforeBindings
                             , beforeSpliceR = prevBeforeSplice } <- getFuture
        return result
      singleHoriLookup bindings sc = fromMaybe mempty $ M.lookup sc bindings

midRes (p, MidIdentifier r symbol) = fmap (fmap $ MidIdentifier r) $
  getsPast (M.lookup p . gensyms) >>= maybe (lookup (r, symbol) p) (return . pure)
midRes (p, MidSplice s) = pure . MidSplice <$> resolveSplice p s
midRes (_, Basic t) = return . pure $ Basic t
midRes (_, Repeated{}) = error $ "Compiler error: Repeated is not a leaf"
midRes (_, Sequenced{}) = error $ "Compiler error: Sequenced is not a leaf"

getScope :: SinglePath -> Resolver spliceBinding spliceResult s pre ScopeInstance
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
defineBefore :: Gen pre => (SinglePath, Range, pre) -> Resolver spliceBinding spliceResult s pre (Result ())
defineBefore (p, r, symbol) = do
  sym <- getGenSym p
  bindingsP <- getsPast afterBindingsR
  modifyBackwards $ \rs@ ~ReverseResolverState{beforeBindingsR} ->
    rs { beforeBindingsR = multiInsert closeSc symbol (r, sym) beforeBindingsR }
  bindingsF <- getsFuture beforeBindingsR
  return . maybe ok mkError $ multiLookup closeSc symbol bindingsP
                          <|> multiLookup closeSc symbol bindingsF
  where
    mkError (r', _) = redefError symbol r r'

defineAfter :: Gen pre => (SinglePath, Range, pre) -> Resolver spliceBinding spliceResult s pre (Result ())
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

getClose :: Monoid m => M.Map ScopeInstance m -> m
getClose = fromMaybe mempty . M.lookup closeSc

{-
Notice that defineVertical makes no overlap check with horizontal bindings,
nor does defineBefore/defineAfter check for overlap with vertical
bindings. The checks are at beginning and end of resolve, and in
midRes before and after calling resolve.
-- TODO: check with previous after bindings, since that's how outside bindings will most likely enter through resolve
-- TODO: the above won't work I think, I think it won't be lazy enough
-}
defineVertical :: Gen pre => (TreePath, (SinglePath, Range, pre)) -> Resolver spliceBinding spliceResult s pre (Result ())
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

lookup :: Gen pre => (Range, pre) -> SinglePath -> Resolver spliceBinding spliceResult s pre (Result GenSym)
lookup (r, symbol) p = do
  sc <- getScope p
  fmap (maybe err pure . asum) . mapM singleScLookup $ scopeChain sc
  where
    err = undefError symbol r
    singleScLookup sc = do
      mAfter <- singleHoriLookup sc <$> getsPast afterBindingsR
      mBefore <- singleHoriLookup sc <$> getsFuture beforeBindingsR
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

getGenSym :: SinglePath -> Resolver spliceBinding spliceResult s pre GenSym
getGenSym p = getsPast $ fromMaybe err . M.lookup p . gensyms
  where
    err = error $ "Compiler error: path " ++ show p ++ " should have a gensym already, but doesn't"

resolveSplice :: (Gen pre, Monoid spliceResult, Monoid spliceBinding) => SinglePath -> s (FixNode s pre) -> Resolver spliceBinding spliceResult s pre (s (FixNode s GenSym))
resolveSplice p s = mdo
  sc <- getScope p
  modifyBackwards $ \rs ->
    rs { beforeSpliceR = mappend beforeSplice `alter` sc $ beforeSpliceR }
  ResolverState{afterBindingsR, inBindingsR, afterSpliceR, resSplice, spliceResult} <- getPast
  let scs = scopeChain sc
      ((beforeSplice, afterSplice), (resolvedSplice, result)) =
        resSplice (getAllPre beforeBindingsR scs <> getAllPre afterBindingsR scs <> getAllIn inBindingsR scs)
                  (getAllSplice beforeSpliceR scs, getAllSplice afterSpliceR scs)
                  s
  ~ReverseResolverState{beforeBindingsR, beforeSpliceR} <- getFuture
  modifyForwards $ \rs ->
    rs { afterSpliceR = mappend afterSplice `alter` sc $ afterSpliceR
       , spliceResult = spliceResult <> result }
  return resolvedSplice
  where
    getAllSplice horizontal scs = (`M.lookup` horizontal) <$> scs
      & catMaybes
      & mconcat
    getAllIn :: Gen pre => VerticalBindings pre -> [ScopeInstance] -> S.Set pre
    getAllIn vertical scs = (`M.lookup` vertical) <$> scs
      & catMaybes
      & fmap (S.fromAscList . fmap fst . filter (any (childOf p . fst) . snd) . M.toAscList)
      & S.unions
    getAllPre horizontal scs = (`M.lookup` horizontal) <$> scs
      & catMaybes
      & fmap M.keysSet
      & S.unions

-- Gen s means that s can be used as a pre symbol in name resolution
class (Ord pre, Show pre) => Gen pre where
  getString :: pre -> String
  gensym :: (SinglePath, pre) -> Resolver spliceBinding spliceResult s pre GenSym
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
