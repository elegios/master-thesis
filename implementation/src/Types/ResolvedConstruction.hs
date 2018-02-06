{-# LANGUAGE RecordWildCards, Rank2Types #-}

module Types.ResolvedConstruction
( Scope(..)
, ResolvedConstruction(..)
, expand
, ExpansionFunction(..)
) where

import Types.Ast (FixNode, MidNodeI)
import Types.Paths (MultiPath, TreeEndPath, TreePath)
import Types.GenSym (GenSym)

data Scope = FarScope | CloseScope | Scope MultiPath TreeEndPath Scope deriving (Show, Eq, Ord)

type Node s = FixNode s GenSym
type MidNode s = MidNodeI (s (Node s)) GenSym

newtype ExpansionFunction = ExpansionFunction { unExpFun :: forall s. Node s -> MidNode s }

data ResolvedConstruction = ResolvedConstruction
  { beforeBindings :: TreeEndPath
  , afterBindings :: TreeEndPath
  , inBindings :: [(TreeEndPath, TreePath)]
  , scopes :: [Scope]
  , _expand :: Maybe ExpansionFunction }

expand :: ResolvedConstruction -> Maybe (Node s -> MidNode s)
expand ResolvedConstruction{_expand} = unExpFun <$> _expand

instance Show ResolvedConstruction where
  show ResolvedConstruction{..} = "ResolvedConstruction{beforeBindings=" ++ show beforeBindings
                                  ++ ", afterBindings=" ++ show afterBindings
                                  ++ ", inBindings=" ++ show inBindings
                                  ++ ", scopes=" ++ show scopes
                                  ++ ", expand=" ++ show (const "<func>" <$> _expand)
                                  ++ "}"
