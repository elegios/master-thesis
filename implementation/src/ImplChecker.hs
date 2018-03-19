{-# LANGUAGE RecordWildCards #-}

module ImplChecker (check, Error(..)) where

import Control.Monad (zipWithM)
import Control.Arrow ((&&&), (***))

import qualified Data.Map as M
import qualified Data.Set as S

import qualified Binding

import Types.Construction (Construction(syntax), SyntaxPattern(..), Repeat(..))
import Types.ResolvedConstruction (ResolvedConstruction, expand)
import Types.Ast (MidNodeI(..), NodeI(..), FixNode(..))
import Types.GenSym (GenSym(..))
import Types.Result (ResultT(..))

type Node s = FixNode s GenSym
type MidNode s = MidNodeI (s (Node s)) GenSym

-- TODO: no check is done on correct usage of fold1, do that here or in construction resolution?

-- First String is name of syntax construction
           -- FakeNode can bind names exposed through Dependency before expansion, but not after
data Error = DependencyError String FakeNode Dependency
           -- FakeNode can see names exported from itself after expansion, can give redef errors
           | SelfDependentError String FakeNode
           -- Something was exported (#bind before, or #bind after) before expansion, but isn't after
           | ExportExpectedError String Export
           -- Regular binding errors, possibly by not exposing an Identifier from the syntax description
           | BindingError String Binding.Error
           deriving (Show, Eq, Ord)

check :: M.Map String (Construction n) -> M.Map String ResolvedConstruction -> S.Set Error
check constrs resConstrs = mconcat . M.elems $
  M.intersectionWithKey (doCheck resConstrs) (expand `M.mapMaybe` resConstrs) (syntax <$> constrs)

data FakeNodeS s = FakeNodeS FakeNode deriving (Show)
data FakeNode = FakeNode FakeKind [Int] deriving (Eq, Ord)
data FakeKind = SyntaxKind | OtherKind deriving (Eq, Ord)

instance Show FakeNode where
  show (FakeNode kind path) = show kind ++ "#" ++ show (reverse path)
instance Show FakeKind where
  show SyntaxKind = "syntax"
  show OtherKind = "other"

doCheck :: M.Map String (ResolvedConstruction) -> String -> (Node FakeNodeS -> MidNode FakeNodeS) -> [SyntaxPattern] -> S.Set Error
doCheck resConstrs constrName _expand pats = S.unions $ inner <$> pairs
  where
    inner (start, expansion) =
      let (startErrs, (startDeps, startExports)) = computeDependencies resConstrs start
          (expansionErrs, (expansionDeps, expansionExports)) = computeDependencies resConstrs expansion
          bindingErrors = S.mapMonotonic (BindingError constrName) $ expansionErrs `S.difference` startErrs
          dependencyErrors = S.unions . M.elems $ depErrIntersect startDeps expansionDeps
          exportErrors = S.mapMonotonic (ExportExpectedError constrName) $ startExports `S.difference` expansionExports
      in S.unions [bindingErrors, dependencyErrors, exportErrors]
    depErrIntersect = M.intersectionWithKey $ \n req sat ->
      let unsatisfied = S.mapMonotonic (DependencyError constrName n) $ req `S.difference` sat
          selfDependent = if any (\case {SyntaxDependency n' _ -> n == n'; _ -> False}) sat
            then S.singleton $ SelfDependentError constrName n
            else S.empty
      in unsatisfied `S.union` selfDependent
    expand n = case _expand n of
      MidNode n' -> FixNode n'
      MidSplice s' -> FixNode $ SyntaxSplice s'
      m' -> error $ "Compiler error: expansion returned non-node: " ++ show m' ++ " in constrName: " ++ show constrName
    pairs = (id &&& expand) . mkNode <$> generateVariations pats
    mkNode children = FixNode Node { name = constrName, children, nodeRange = mempty }

generateVariations :: [SyntaxPattern] -> [[MidNode FakeNodeS]]
generateVariations pats = zipWithM gen pats (prepend [])

type VariationM a = [a]

gen :: SyntaxPattern -> [Int] -> VariationM (MidNode FakeNodeS)
gen IdentifierPat{} path = return $ fakeIdentifier path
gen SyntaxPat{} path = return $ fakeNode SyntaxKind path
gen (RepeatPat pat rep) path = do
  reps <- repNum rep
  children <- sequence . take reps $ gen pat <$> (prepend path)
  return $ Repeated rep children
  where
    repNum StarRep = [0..2]
    repNum PlusRep = [1..3]
    repNum QuestionRep = [0..1]
gen (SequencePat pats) path = Sequenced mempty <$> zipWithM gen pats (prepend path)
gen (NamedPat _ pat) path = gen pat path
gen _ path = return $ fakeNode OtherKind path

fakeNode :: FakeKind -> [Int] -> MidNode FakeNodeS
fakeNode kind path = MidSplice . FakeNodeS $ FakeNode kind path

fakeIdentifier :: [Int] -> MidNode FakeNodeS
fakeIdentifier path = MidIdentifier mempty $ GenSym (show path) 1

prepend :: [Int] -> [[Int]]
prepend path = (:path) <$> [0..]

data Direction = After | Before deriving (Show, Eq, Ord)
data Dependency = IdentifierDependency GenSym
                | SyntaxDependency FakeNode Direction
                deriving (Show, Eq, Ord)
data Export = SyntaxExport Direction FakeNode
            | IdentifierExport Direction GenSym
            deriving (Show, Eq, Ord)

computeDependencies :: M.Map String ResolvedConstruction -> Node FakeNodeS -> (S.Set Binding.Error, (M.Map FakeNode (S.Set Dependency), S.Set Export))
computeDependencies _ (FixNode (SyntaxSplice (FakeNodeS s))) =
  (S.empty, (M.singleton s S.empty, S.fromList [SyntaxExport Before s, SyntaxExport After s]))
computeDependencies resConstrs n =
  toErrors *** toExports $ Binding.resolveNamesS resSplice resConstrs n
  where
    toErrors (Data _) = S.empty
    toErrors (Error es) = S.fromList es
    toExports Binding.SpliceBindings{..} = (result, before `S.union` after)
      where
        before = S.mapMonotonic (IdentifierExport Before) beforeBinding `S.union` S.map depToExport beforeSplice
        after = S.mapMonotonic (IdentifierExport After) afterBinding `S.union` S.map depToExport afterSplice
        depToExport (SyntaxDependency n dir) = SyntaxExport dir n
        depToExport dep = error $ "Compiler error: expected SyntaxDependency, got: " ++ show dep
    resSplice symbols (beforeSplice, afterSplice) (FakeNodeS n) =
      (newBindings, (FakeNodeS n, result))
      where
        newBindings = case n of
          FakeNode SyntaxKind _ -> (toDep Before, toDep After)
          _ -> (S.empty, S.empty)
        toDep = S.singleton . SyntaxDependency n
        result = M.singleton n $ S.unions [beforeSplice, afterSplice, S.mapMonotonic IdentifierDependency symbols]