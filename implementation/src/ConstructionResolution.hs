{-# LANGUAGE ApplicativeDo #-}

module ConstructionResolution(resolve, Error(..)) where

import Control.Arrow (second)
import Control.Monad ((>=>), forM)
import Data.Foldable (foldl')

import qualified Data.Map as M
import qualified Data.Set as S

import Types.Construction
import Types.Paths
import Types.ResolvedConstruction
import Types.Result

-- data Scope = FarScope | CloseScope | Scope MultiPath TreeEndPath Scope deriving (Show, Eq, Ord)
-- data ResolvedConstruction = ResolvedConstruction
--   { beforeBindings :: TreeEndPath
--   , afterBindings :: TreeEndPath
--   , inBindings :: [(TreeEndPath, TreePath)]
--   , scopes :: [Scope] }
--   deriving (Show)

{-
- Find name to path translation
- Construct scopes
- Collect before and after (could/should check that they are in CloseScope)
- Collect inBindings (could/should check that target area has source area in scope)

-- TODO: should check that names used in scope are disjoint (except for ) (?)
-- TODO: should check that names pointing to binders actually point to identifiers
-}

-- TODO: actual real errors here
-- NOTE: first String is the name of the construction
data Error = NameAlreadyDefined String String
           | UndefinedName String String
           | OverlappingNames String
           | InnerScopeBiggerThanOuter String
           deriving (Show) -- TODO: probably use a nicer instance for Show, or some other typeclass

type Result = ResultT [Error]

resolve :: Construction n -> Result ResolvedConstruction
resolve Construction{name=constrName, syntax, extraData} = do
  nameToPath <- findNames constrName syntax
  let findPath name = maybe (undefError name) pure $ M.lookup name nameToPath
      toMultiPaths = sequenceA . fmap findPath . S.toList . S.fromList
      toTreeEnd = maybe overlapError pure . mergeToTreeEnd
      check outerRepMp repMp = if repMp `childOf` outerRepMp
        then pure ()
        else Error . pure $ InnerScopeBiggerThanOuter constrName
      resolveScopeData outerRepMp outerSc (ScopeData mRepMp content scds) = do
        repMp' <- maybe (pure outerRepMp) findPath mRepMp
        content' <- toMultiPaths content >>= toTreeEnd
        let sc = Scope repMp' content' outerSc
        check outerRepMp repMp'
        fmap ((sc:) . concat) $ resolveScopeData repMp' sc `mapM` scds
  beforePaths <- toMultiPaths beforeBindings >>= toTreeEnd
  afterPaths <- toMultiPaths afterBindings >>= toTreeEnd
  inPathPairs <- forM bindingData $ \(binders, bound) -> do
    binders' <- toMultiPaths binders >>= toTreeEnd
    bound' <- foldl' mergePaths nowhere <$> toMultiPaths bound
    return (binders', bound')
  scopes <- fmap concat $ resolveScopeData here CloseScope `mapM` scopeData
  return ResolvedConstruction
    { beforeBindings = beforePaths
    , afterBindings = afterPaths
    , inBindings = inPathPairs
    , scopes = scopes }
  where
    undefError name = Error . pure $ UndefinedName constrName name
    overlapError = Error . pure $ OverlappingNames constrName
    ExtraData{beforeBindings, afterBindings, bindingData, scopeData} = extraData

findNames :: String -> [SyntaxPattern] -> Result (M.Map String MultiPath)
findNames constrName = sequenceA . M.fromListWithKey dupErr . fmap (second pure) . multiPat
  where
    multiPat = zip [0..] >=> \(i, pat) -> second (prependMulti $ Just i) <$> recur pat
    recur (RepeatPat pat _) = second (prependMulti Nothing) <$> recur pat
    recur (SequencePat pats) = multiPat pats
    recur (NamedPat name pat) = (name, here) : recur pat
    recur _ = []
    dupErr name _ _ = Error . pure $ NameAlreadyDefined constrName name
