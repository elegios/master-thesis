module Ambiguity (ambiguities) where

import Data.List (transpose, nub)
import Control.Arrow ((&&&))

import Types.Lexer (Ranged(..), Range, Token(..))
import Types.Construction (Repeat(..))
import Types.Ast (NodeI(Node, name), MidNodeI(..))

type Node = NodeI String
type MidNode = MidNodeI String

ambiguities :: [Node] -> [(Range, [[SimpleRepr]])]
ambiguities nodes = ambig $ midWrap <$> nodes
  where
    r = range nodes
    midWrap n = MidNode $ Node "*top*" [("*content*", MidNode n)] r

-- invariant: all items in the argument list share a SimpleRepr
ambig :: [MidNode] -> [(Range, [[SimpleRepr]])]
ambig [] = []
ambig nodes@(node:_) =
  let cs = children <$> nodes
      childLengthsEqual = allEq $ length <$> cs
      simpleCs = fmap toSimple <$> cs
      similarColumns = allEq <$> transpose simpleCs
      nonSimilar = keepOnly (not <$> similarColumns) <$> simpleCs
      similar = keepOnly similarColumns <$> cs
  in case node of
       Repeated{} | not childLengthsEqual -> return $ singleton simpleCs
       _ | null $ head nonSimilar -> concat $ ambig <$> transpose similar
       _ -> singleton nonSimilar : concat (ambig <$> transpose similar)
  where
    singleton = range &&& nub

toSimple :: MidNode -> SimpleRepr
toSimple (MidNode n@Node{name}) = SimpleRepr (NodeTag name) (range n)
toSimple (MidIdentifier r i) = SimpleRepr IdentifierTag r
toSimple (Basic t@IdentifierTok{}) = SimpleRepr IdentifierTag (range t)
toSimple (Basic t@SymbolTok{}) = SimpleRepr SymbolTag (range t)
toSimple (Basic t@StringTok{}) = SimpleRepr StringTag (range t)
toSimple (Basic t@IntegerTok{}) = SimpleRepr IntegerTag (range t)
toSimple (Basic t@FloatTok{}) = SimpleRepr FloatTag (range t)
toSimple n@(Repeated r _) = SimpleRepr (RepeatTag r) (range n)
toSimple (Sequenced r _) = SimpleRepr SequencedTag r

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (a:as) = all (a ==) as

keepOnly :: [Bool] -> [a] -> [a]
keepOnly (True:bs) (a:as) = a : keepOnly bs as
keepOnly (False:bs) (_:as) = keepOnly bs as
keepOnly _ _ = []

children :: MidNode -> [MidNode]
children (MidNode (Node _ children _)) = snd <$> children
children MidIdentifier{} = []
children Basic{} = []
children (Repeated _ cs) = cs
children (Sequenced _ cs) = snd <$> cs

data SimpleRepr = SimpleRepr Tag Range deriving (Show, Eq)
data Tag = NodeTag String
         | IdentifierTag
         | SymbolTag
         | StringTag
         | IntegerTag
         | FloatTag
         | RepeatTag Repeat
         | SequencedTag
         deriving (Show, Eq)

instance Ranged SimpleRepr where
  range (SimpleRepr _ r) = r
