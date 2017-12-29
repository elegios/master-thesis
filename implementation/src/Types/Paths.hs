{-# LANGUAGE ViewPatterns #-}

module Types.Paths
( SinglePath -- Path to a single element in ast
, MultiPath -- Path to a single element in construction, multiple in ast
, TreeEndPath -- Merge of multiple child-disjoint MultiPaths
, TreePath -- Merge of any two paths
, singlePath
, multiPath
, fromMultiPath
, calculatePathInstance
, limitPathInstance
, prepend
, prependMulti
, here
, nowhere
, oneOf
, childOf
, childrenOverlapping
, childrenDisjoint
, mergeToTreeEnd
, mergePaths
) where

import Data.Function ((&))
import Data.List (foldl')
import Data.Maybe (isNothing, catMaybes, maybeToList, fromMaybe, fromJust)

import Control.Arrow (second)

import qualified Data.Map as M

newtype SinglePath = SinglePath TreePath deriving (Path, Eq, Ord)
newtype MultiPath = MultiPath TreePath deriving (Path, Eq, Ord)
newtype TreeEndPath = TreeEndPath TreePath deriving (Path, Eq, Show, Ord)
data TreePath = TreePath (M.Map (Maybe Int) TreePath) Bool deriving (Eq, Show, Ord)

instance Show SinglePath where
  show (SinglePath tp) = show $ fromJust <$> toListUnsafe tp

instance Show MultiPath where
  show (MultiPath tp) = show $ toListUnsafe tp

fromMultiPath :: MultiPath -> [Maybe Int]
fromMultiPath = toListUnsafe . view

singlePath :: [Int] -> SinglePath
singlePath = foldr prepend here

multiPath :: [Maybe Int] -> MultiPath
multiPath = foldr prepend' . MultiPath $ TreePath M.empty True
  where
    prepend' i (MultiPath tp) = MultiPath $ TreePath (M.singleton i tp) False

calculatePathInstance :: MultiPath -> SinglePath -> [Int]
calculatePathInstance (MultiPath mp) (SinglePath sp) = toListUnsafe mp `zip` toListUnsafe sp
  & filter (isNothing . fst)
  & fmap snd
  & catMaybes

-- NOTE: will limit by cutting off the end of the instance, rather than the beginning
limitPathInstance :: MultiPath -> [Int] -> [Int]
limitPathInstance (MultiPath mp) inst = toListUnsafe mp
  & filter isNothing
  & zip inst
  & fmap fst

-- NOTE: the TreePath argument should be a SinglePath or a MultiPath, but this is not checked
toListUnsafe :: TreePath -> [Maybe Int]
toListUnsafe (TreePath _ True) = []
toListUnsafe (TreePath m False) = uncurry (:) . second toListUnsafe $ M.findMin m

prepend :: Int -> SinglePath -> SinglePath
prepend _ p | p == nowhere = p
prepend i (SinglePath tp) = SinglePath $ TreePath (M.singleton (Just i) tp) False

prependMulti :: Maybe Int -> MultiPath -> MultiPath
prependMulti _ mp | mp == nowhere = mp
prependMulti i (MultiPath tp) = MultiPath $ TreePath (M.singleton i tp) False

here :: Path p => p
here = unview $ TreePath M.empty True

nowhere :: Path p => p
nowhere = unview $ TreePath M.empty False

mergePaths :: (Path p, Path p') => p -> p' -> TreePath
mergePaths (view -> TreePath m1 b1) (view -> TreePath m2 b2) =
  TreePath (M.unionWith mergePaths m1 m2) (b1 || b2)

mergeToTreeEnd :: Path p => [p] -> Maybe TreeEndPath
mergeToTreeEnd = toTreeEnd . foldl' mergePaths nowhere . fmap view

-- TODO: this might not be correct if the path has Nothing components
toTreeEnd :: TreePath -> Maybe TreeEndPath
toTreeEnd tp = if isTreeEnd tp then Just $ TreeEndPath tp else Nothing
  where
    isTreeEnd (TreePath m end) = (end && M.null m)
                              || (not end && all isTreeEnd m)

oneOf :: (Path p, Path p') => p -> p' -> Bool
oneOf (view -> TreePath m b) (view -> TreePath m' b') =
  and $ here : (step <$> M.toList m)
  where
    here = not b || b'
    step (k@Just{}, p) = inner k p || inner Nothing p
    step (Nothing, p) = inner Nothing p
    inner k p = fromMaybe False $ oneOf p <$> M.lookup k m'

childOf :: (Path p, Path p') => p -> p' -> Bool
childOf _ (view -> TreePath m' True) = True
childOf (view -> TreePath m False) (view -> TreePath m' _) =
  and $ step <$> M.toList m
  where
    step (k@Just{}, p) = inner k p || inner Nothing p
    step (Nothing, p) = inner Nothing p
    inner k p = fromMaybe False $ childOf p <$> M.lookup k m'
childOf _ _ = False

-- there is at least one SinglePath p such that p `childOf` p1 && p `childOf` p2
childrenOverlapping :: (Path p, Path p') => p -> p' -> Bool
childrenOverlapping (view -> p1) (view -> p2) = recur p1 p2
  where
    recur (TreePath _ True) _ = True
    recur _ (TreePath _ True) = True
    recur (TreePath m1 False) (TreePath m2 False) =
      or normal || or (star m1 m2) || or (star m2 m1)
      where
        normal = M.intersectionWith recur m1 m2
        star m1 m2 = recur <$> maybeToList (M.lookup Nothing m1) <*> M.elems (M.delete Nothing m2)

childrenDisjoint :: (Path p, Path p') => p -> p' -> Bool
childrenDisjoint p1 p2 = not $ childrenOverlapping p1 p2

class Path p where
  view :: p -> TreePath
  -- This function is obviously very unsafe, so it should not be exported, and be used with care
  unview :: TreePath -> p

instance Path TreePath where
  view = id
  unview = id
