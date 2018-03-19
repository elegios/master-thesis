{-# LANGUAGE ApplicativeDo, RecordWildCards, ScopedTypeVariables, Rank2Types #-}

module ConstructionResolution(resolve, Error(..)) where

import Control.Arrow (second)
import Control.Monad ((>=>), forM, foldM)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Data.List (elemIndex)
import qualified Unsafe.Coerce

import Control.Monad.Reader (Reader, runReader, ask, local, reader)

import qualified Data.Map as M
import qualified Data.Set as S

import Types.Construction
import Types.Paths
import Types.ResolvedConstruction
import Types.Result
import Types.GenSym
import Types.Ast

{-
-- TODO: should check that names used in #scope are disjoint (except for list specifiers, which must be children of parent list specifiers) (?)
-- TODO: should check that names pointing to binders actually point to identifiers
-}

-- NOTE: first String is the name of the construction
data Error = NameAlreadyDefined String String
           | UndefinedName String String
           | OverlappingNames String
           | InnerScopeBiggerThanOuter String
           | SingleNameExpected String String
           | PoorFoldScoping String String
           deriving (Show) -- TODO: probably use a nicer instance for Show, or some other typeclass

type Result = ResultT [Error]

data Helpers = Helpers { findPath :: String -> Result MultiPath
                       , toMultiPaths :: [String] -> Result [MultiPath]
                       , toTreeEnd :: [MultiPath] -> Result TreeEndPath
                       , constrName :: String }

resolve :: Construction SplicedNode -> Result ResolvedConstruction
resolve Construction{name=constrName, syntax, extraData, implementation} = do
  nameToPath <- findNames constrName syntax
  let findPath name = maybe (undefError name) pure $ M.lookup name nameToPath
      toMultiPaths = sequenceA . fmap findPath . S.toList . S.fromList
      toTreeEnd = maybe overlapError pure . mergeToTreeEnd
      helpers = Helpers{..}
  beforePaths <- toMultiPaths beforeBindings >>= toTreeEnd
  afterPaths <- toMultiPaths afterBindings >>= toTreeEnd
  inPathPairs <- forM bindingData $ \(binders, bound) -> do
    binders' <- toMultiPaths binders >>= toTreeEnd
    bound' <- foldl' mergePaths nowhere <$> toMultiPaths bound
    return (binders', bound')
  scopes <- fmap concat $ resolveScopeData helpers `mapM` scopeData
  expand <- sequenceA $ mkExpand helpers <$> implementation
  return ResolvedConstruction
    { beforeBindings = beforePaths
    , afterBindings = afterPaths
    , inBindings = inPathPairs
    , scopes = scopes
    , _expand = expand }
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

resolveScopeData :: Helpers -> ScopeData -> Result [Scope]
resolveScopeData Helpers{..} = recur here CloseScope
  where
    recur outerRepMp outerSc (ScopeData mRepMp content scds) = do
      repMp' <- maybe (pure outerRepMp) findPath mRepMp
      content' <- toMultiPaths content >>= toTreeEnd
      let sc = Scope repMp' content' outerSc
      check outerRepMp repMp'
      fmap ((sc:) . concat) $ recur repMp' sc `mapM` scds
    check outerRepMp repMp = if repMp `childOf` outerRepMp
      then pure ()
      else Error . pure $ InnerScopeBiggerThanOuter constrName

type Node s = FixNode s GenSym
type SplicedNode = FixNode Splice String
type MidNode s = MidNodeI (s (Node s)) GenSym
type SplicedMidNode = MidNodeI (Splice SplicedNode) String

data ExpanderState s = ExpanderState
  { node :: Node s
  , accs :: [MidNode s]
  , inst :: [Int] }
type Expander s a = Reader (ExpanderState s) a

{-
The unsafe coercion below *should* be safe, but it is definitely the first suspect in case of error.
unsafeCoerce :: (Node s0 -> MidNode s0) -> (forall s. Node s -> MidNode s)
should be the type signature of unsafeCoerce below, where s0 is some unspecified type of kind (* -> *)
The function itself never uses a value of type (s a) in any non-atomic way, i.e.
while a value of such a type is used it is merely placed in another value of
type (Node s) or (MidNode s).
Thus the code does not use the representation of (s a), at least not directly.
It is possible that an error would occur if the compiler has to know the
precise representation of (s a) to put it inside (Node s) or (MidNode s), but
I believe that should not be the case, since the default representation should
be boxed and lazy.
-}

mkExpand :: Helpers -> Splice SplicedNode -> Result ExpansionFunction
mkExpand h n = do
  expander <- recurS [] here h n
  return $ ExpansionFunction $ Unsafe.Coerce.unsafeCoerce $ \node ->
    runReader expander $ mkExpanderState node
  where
    mkExpanderState node = ExpanderState { node = node, accs = [], inst = [] }

recurS :: forall s. [String] -> MultiPath -> Helpers -> Splice SplicedNode -> Result (Expander s (MidNode s))
recurS accNames instPath h@Helpers{..} = \case
  Syntax n -> recurN n
  Simple s -> followSinglePath s
  Fold dir list acc f start -> do
    start' <- recurS accNames instPath h start
    let dir' = dirF dir
    (instPath', insts) <- second (fmap dir') <$> findInstances list
    f' <- foldF <$> recurS (acc : accNames) instPath' h f
    return $ do
      s <- start'
      insts >>= foldM f' s
  Fold1 dir list acc f -> do
    let dir' = dirF dir
    (instPath', insts) <- second (fmap dir') <$> findInstances list
    f' <- foldF <$> recurS (acc : accNames) instPath' h f
    start <- followSinglePath' instPath' list
    return $ insts >>= \case
      inst : insts -> do
        start' <- local (\es -> es {inst}) $ start
        foldM f' start' insts
      [] -> error $ "Compiler error: fold1 used on something with zero instances"
  where
    dirF FoldLeft = id
    dirF FoldRight = reverse
    foldF :: Expander s (MidNode s) -> MidNode s -> [Int] -> Expander s (MidNode s)
    foldF f' acc inst =
      local (\es@ExpanderState{accs} -> es { accs = acc : accs, inst }) f'

    recurN :: SplicedNode -> Result (Expander s (MidNode s))
    recurN (FixNode n@Node{children}) = mapM recurM children
      & fmap sequence
      & fmap (fmap $ \cs -> MidNode $ n {children = cs})
    recurN (FixNode (SyntaxSplice s)) = recurS accNames instPath h s

    recurM :: SplicedMidNode -> Result (Expander s (MidNode s))
    recurM (MidNode n) = recurN $ FixNode n
    recurM (MidIdentifier r symbol) = pure . return . MidIdentifier r $ expandGenSym symbol
    recurM (MidSplice s) = recurS accNames instPath h s
    recurM (Basic t) = pure . return $ Basic t
    recurM (Repeated rep ms) = fmap (Repeated rep) . sequence <$> mapM recurM ms
    recurM (Sequenced r ms) = fmap (Sequenced r) . sequence <$> mapM recurM ms

    followSinglePath = followSinglePath' instPath
    followSinglePath' :: MultiPath -> String -> Result (Expander s (MidNode s))
    followSinglePath' instPath name = case elemIndex name accNames of
      Just i -> pure . reader $ (!! i) . accs
      Nothing -> do
        mp <- findPath name
        if mp `isSpecializedBy` instPath
          then pure $ followSPN <$> reader node <*> (catMaybes <$> specialize (fromMultiPath mp))
          else Error . pure $ SingleNameExpected constrName name
    followSPN (FixNode Node{children}) (i:p) = followSPM p $ children !! i
    -- followSPN n p = error $ "Compiler error: malformed path or node, p: " ++ show p ++ ", n: " ++ show n
    followSPN n p = error $ "Compiler error: malformed path or node, p: " ++ show p
    followSPM [] m = m
    followSPM (i:p) (Repeated _ ms) = followSPM p $ ms !! i
    followSPM (i:p) (Sequenced _ ms) = followSPM p $ ms !! i
    -- followSPM p m = error $ "Compiler error: malformed path or midnode, p: " ++ show p ++ ", m: " ++ show m
    followSPM p m = error $ "Compiler error: malformed path or midnode, p: " ++ show p
    specialize :: [Maybe Int] -> Expander s [Maybe Int]
    specialize mp = recur mp <$> reader inst
      where
        recur [] _ = []
        recur mp [] = mp
        recur (Nothing:mp) (i:inst) = Just i : recur mp inst
        recur (i@Just{}:mp) inst = i : recur mp inst
    findInstances :: forall. String -> Result (MultiPath, Expander s [[Int]])
    findInstances name = do
      mp <- findPath name
      if mp `childOf` instPath
        then pure . (mp, ) $ do
          ExpanderState{node,inst} <- ask
          return $ calculateInstancesN inst (fromMultiPath mp) node
        else Error . pure $ PoorFoldScoping constrName name
    calculateInstancesN inst (Just i:mp) (FixNode Node{children}) =
      calculateInstancesM inst mp $ children !! i
    -- calculateInstancesN inst mp n = error $ "Compiler error: malformed instance, path, or node, inst: " ++ show inst ++ ", mp: " ++ show mp ++ ", n: " ++ show n
    calculateInstancesN inst mp n = error $ "Compiler error: malformed instance, path, or node, inst: " ++ show inst
    calculateInstancesM [] [] _ = [[]]
    calculateInstancesM [] (Nothing:mp) (Repeated _ ms) =
      calculateInstancesM [] mp <$> ms
      & zipWith (fmap . (:)) [0..]
      & concat
    calculateInstancesM (i:inst) (Nothing:mp) (Repeated _ ms) =
      fmap (i:) . calculateInstancesM inst mp $ ms !! i
    calculateInstancesM inst (Just i:mp) (Sequenced _ ms) =
      calculateInstancesM inst mp $ ms !! i
    -- calculateInstancesM inst mp m = error $ "Compiler error: malformed instance or path, inst: " ++ show inst ++ ", mp: " ++ show mp ++ ", m: " ++ show m
    calculateInstancesM inst mp m = error $ "Compiler error: malformed instance or path, inst: " ++ show inst
