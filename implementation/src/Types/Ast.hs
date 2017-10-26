module Types.Ast
( Node(..)
, MidNode(..)
, pretty
, prettyShow
) where

import Data.Function ((&))
import Data.List (intercalate)

import Text.PrettyPrint ((<+>))
import qualified Text.PrettyPrint as P

import Types.Construction (Repeat(..))
import Types.Lexer (Ranged(..), Range, Token)

data Node = Node
  { name :: String
  , children :: [(String, MidNode)]
  , nodeRange :: Range }

data MidNode = MidNode Node
             | Basic Token
             | Repeated Repeat [MidNode]
             | Sequenced [(String, MidNode)] Range

instance Show Node where
  show Node{name, children, nodeRange} = name ++ "{" ++ show nodeRange ++ "}" ++ showNamed children

instance Show MidNode where
  show (MidNode n) = show n
  show (Basic t) = show t
  show (Repeated _ mids) = "[" ++ intercalate ", " (show <$> mids) ++ "]"
  show (Sequenced named r) = "{" ++ show r ++ "}" ++ showNamed named

instance Ranged Node where
  range = nodeRange

instance Ranged MidNode where
  range (MidNode n) = range n
  range (Basic t) = range t
  range (Repeated _ ns) = mconcat $ range <$> ns
  range (Sequenced _ r) = r

showNamed :: [(String, MidNode)] -> String
showNamed named = "(" ++ intercalate ", " (arg <$> named) ++ ")"
  where
    arg (name, node) = name ++ " = " ++ show node

pretty :: Node -> P.Doc
pretty (Node n cs _) = P.sep [P.text n, prettyNamed cs]
  where
    prettyNamed cs = cs
      & fmap (\(n, mid) -> P.text n <+> P.equals <+> prettyMid mid)
      & P.punctuate P.comma
      & P.vcat & P.parens
    namedPretty (n, mid) = P.text n <+> P.equals <+> prettyMid mid
    prettyMid (MidNode n) = pretty n
    prettyMid (Basic t) = P.text $ show t
    prettyMid (Repeated _ mids) = prettyMid <$> mids
      & P.punctuate P.comma
      & P.sep & P.brackets
    prettyMid (Sequenced named _) = prettyNamed named

prettyShow :: Node -> String
prettyShow = P.render . pretty
