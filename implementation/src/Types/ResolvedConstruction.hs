{-# LANGUAGE RecordWildCards #-}

module Types.ResolvedConstruction (Scope(..), ResolvedConstruction(..)) where

import Types.Construction (NoSplice)
import Types.Ast (FixNode, MidNodeI)
import Types.Paths (MultiPath, TreeEndPath, TreePath)
import Types.GenSym (GenSym)

data Scope = FarScope | CloseScope | Scope MultiPath TreeEndPath Scope deriving (Show, Eq, Ord)

type Node = FixNode NoSplice GenSym
type MidNode = MidNodeI (NoSplice Node) GenSym

data ResolvedConstruction = ResolvedConstruction
  { beforeBindings :: TreeEndPath
  , afterBindings :: TreeEndPath
  , inBindings :: [(TreeEndPath, TreePath)]
  , scopes :: [Scope]
  , expand :: Maybe (Node -> MidNode) }

instance Show ResolvedConstruction where
  show ResolvedConstruction{..} = "ResolvedConstruction{beforeBindings=" ++ show beforeBindings
                                  ++ ", afterBindings=" ++ show afterBindings
                                  ++ ", inBindings=" ++ show inBindings
                                  ++ ", scopes=" ++ show scopes
                                  ++ ", expand=" ++ show (const "<func>" <$> expand)
                                  ++ "}"
