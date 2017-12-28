module Types.ResolvedConstruction where

import Types.Paths (MultiPath, TreeEndPath, TreePath)

data Scope = FarScope | CloseScope | Scope MultiPath TreeEndPath Scope deriving (Show, Eq, Ord)

data ResolvedConstruction = ResolvedConstruction
  { beforeBindings :: TreeEndPath
  , afterBindings :: TreeEndPath
  , inBindings :: [(TreeEndPath, TreePath)]
  , scopes :: [Scope] }
  deriving (Show)
