{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

module Types.Ast
( NodeI(..)
, MidNodeI(..)
, FixNode(..)
, pretty
, prettyShow
) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Data.Function ((&))
import Data.List (intercalate)

import Text.PrettyPrint ((<+>))
import qualified Text.PrettyPrint as P

import Types.Construction (Repeat(..))
import Types.Lexer (Ranged(..), Range, Token(..))

newtype FixNode s i = FixNode { unSplice :: NodeI (s (FixNode s i)) i} deriving (Ranged)
deriving instance (Show (s (FixNode s i)), Show i) => Show (FixNode s i)
deriving instance (Eq (s (FixNode s i)), Eq i) => Eq (FixNode s i)
deriving instance (NFData (s (FixNode s i)), NFData i) => NFData (FixNode s i)
data NodeI s i = Node
  { name :: String
  , children :: [MidNodeI s i]
  , nodeRange :: Range }
  | SyntaxSplice s
  deriving (Eq, Generic)
instance (NFData s, NFData i) => NFData (NodeI s i)

data MidNodeI s i = MidNode (NodeI s i)
                  | MidIdentifier Range i
                  | MidSplice s
                  | Basic Token
                  | Repeated Repeat [MidNodeI s i]
                  | Sequenced Range [MidNodeI s i]
                  deriving (Eq, Generic)
instance (NFData s, NFData i) => NFData (MidNodeI s i)

instance (Show s, Show i) => Show (NodeI s i) where
  show Node{name, children, nodeRange} = name ++ "{" ++ show nodeRange ++ "}" ++ showNamed children
  show (SyntaxSplice s) = "splice(" ++ show s ++ ")"

instance (Show s, Show i) => Show (MidNodeI s i) where
  show (MidNode n) = show n
  show (MidIdentifier _ i) = "ident(" ++ show i ++ ")"
  show (MidSplice s) = "splice(" ++ show s ++ ")"
  show (Basic t) = show t
  show (Repeated _ mids) = "[" ++ intercalate ", " (show <$> mids) ++ "]"
  show (Sequenced r named) = "{" ++ show r ++ "}" ++ showNamed named

instance Ranged (NodeI s i) where
  range Node{nodeRange} = nodeRange
  range (SyntaxSplice _) = mempty

instance Ranged (MidNodeI s i) where
  range (MidNode n) = range n
  range (MidIdentifier r _) = r
  range (MidSplice _) = mempty
  range (Basic t) = range t
  range (Repeated _ ns) = mconcat $ range <$> ns
  range (Sequenced r _) = r

-- TODO: not an accurate name at this point
showNamed :: (Show s, Show i) => [MidNodeI s i] -> String
showNamed named = "(" ++ intercalate ", " (show <$> named) ++ ")"

pretty :: (Show (s (FixNode s i)), Show i) => FixNode s i -> P.Doc
pretty (FixNode n) = case n of
  Node n cs _ -> P.sep [P.text n, prettyNamed cs]
  SyntaxSplice _ -> P.text $ show n
  where
    -- TODO: not an accurate name at this point
    prettyNamed cs = cs
      & filter isInteresting
      & fmap prettyMid
      & P.punctuate P.comma
      & P.vcat & P.parens
    namedPretty (n, mid) = P.text n <+> P.equals <+> prettyMid mid
    prettyMid (MidNode n) = pretty $ FixNode n
    prettyMid i@MidIdentifier{} = P.text $ show i
    prettyMid s@MidSplice{} = P.text $ show s
    prettyMid (Basic t) = P.text $ show t
    prettyMid (Repeated _ mids) = prettyMid <$> mids
      & P.punctuate P.comma
      & P.sep & P.brackets
    prettyMid (Sequenced _ named) = prettyNamed named

isInteresting :: MidNodeI s i -> Bool
isInteresting (Basic IdentifierTok{}) = False
isInteresting (Basic SymbolTok{}) = False
isInteresting _ = True

prettyShow :: (Show (s (FixNode s i)), Show i) => FixNode s i -> String
prettyShow = P.render . pretty
