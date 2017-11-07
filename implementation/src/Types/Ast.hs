module Types.Ast
( NodeI(..)
, MidNodeI(..)
, pretty
, prettyShow
) where

import Data.Function ((&))
import Data.List (intercalate)

import Text.PrettyPrint ((<+>))
import qualified Text.PrettyPrint as P

import Types.Construction (Repeat(..))
import Types.Lexer (Ranged(..), Range, Token)

data NodeI i = Node
  { name :: String
  , children :: [(String, MidNodeI i)]
  , nodeRange :: Range }

data MidNodeI i = MidNode (NodeI i)
                | MidIdentifier Range i
                | SyntaxSplice Range String
                | Basic Token
                | Repeated Repeat [MidNodeI i]
                | Sequenced Range [(String, MidNodeI i)]

instance Show i => Show (NodeI i) where
  show Node{name, children, nodeRange} = name ++ "{" ++ show nodeRange ++ "}" ++ showNamed children

instance Show i => Show (MidNodeI i) where
  show (MidNode n) = show n
  show (MidIdentifier _ i) = "ident(" ++ show i ++ ")"
  show (SyntaxSplice _ i) = "splice(" ++ show i ++ ")"
  show (Basic t) = show t
  show (Repeated _ mids) = "[" ++ intercalate ", " (show <$> mids) ++ "]"
  show (Sequenced r named) = "{" ++ show r ++ "}" ++ showNamed named

instance Ranged (NodeI i) where
  range = nodeRange

instance Ranged (MidNodeI i) where
  range (MidNode n) = range n
  range (MidIdentifier r _) = r
  range (SyntaxSplice r _) = r
  range (Basic t) = range t
  range (Repeated _ ns) = mconcat $ range <$> ns
  range (Sequenced r _) = r

showNamed :: Show i => [(String, MidNodeI i)] -> String
showNamed named = "(" ++ intercalate ", " (arg <$> named) ++ ")"
  where
    arg (name, node) = name ++ " = " ++ show node

pretty :: Show i => NodeI i -> P.Doc
pretty (Node n cs _) = P.sep [P.text n, prettyNamed cs]
  where
    prettyNamed cs = cs
      & fmap (\(n, mid) -> P.text n <+> P.equals <+> prettyMid mid)
      & P.punctuate P.comma
      & P.vcat & P.parens
    namedPretty (n, mid) = P.text n <+> P.equals <+> prettyMid mid
    prettyMid (MidNode n) = pretty n
    prettyMid i@MidIdentifier{} = P.text $ show i
    prettyMid i@SyntaxSplice{} = P.text $ show i
    prettyMid (Basic t) = P.text $ show t
    prettyMid (Repeated _ mids) = prettyMid <$> mids
      & P.punctuate P.comma
      & P.sep & P.brackets
    prettyMid (Sequenced _ named) = prettyNamed named

prettyShow :: Show i => NodeI i -> String
prettyShow = P.render . pretty
