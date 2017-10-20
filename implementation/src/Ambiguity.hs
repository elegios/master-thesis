module Ambiguity (ambiguities) where

import Data.List (transpose, nub)
import Control.Arrow ((&&&))

import Lexer (Ranged(..), Range, Token(..))
import BootParser (Repeat(..))
import GrammarGenerator (Node(Node, name), MidNode(..))

ambiguities :: [Node] -> [(Range, [[SimpleRepr]])]
ambiguities nodes = ambig $ midWrap <$> nodes
  where
    r = range nodes
    midWrap n = MidNode $ Node "*top*" [("*content*", MidNode n)] r

-- invariant: all items in the argument list share a SimpleRepr
ambig :: [MidNode] -> [(Range, [[SimpleRepr]])]
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
toSimple (Basic t@IdentifierTok{}) = SimpleRepr IdentifierTag (range t)
toSimple (Basic t@StringTok{}) = SimpleRepr StringTag (range t)
toSimple (Basic t@IntegerTok{}) = SimpleRepr IntegerTag (range t)
toSimple (Basic t@FloatTok{}) = SimpleRepr FloatTag (range t)
toSimple n@(Repeated r _) = SimpleRepr (RepeatTag r) (range n)
toSimple (Sequenced _ r) = SimpleRepr SequencedTag r

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (a:as) = all (a ==) as

keepOnly :: [Bool] -> [a] -> [a]
keepOnly [] [] = []
keepOnly (True:bs) (a:as) = a : keepOnly bs as
keepOnly (False:bs) (_:as) = keepOnly bs as

children :: MidNode -> [MidNode]
children (MidNode (Node _ children _)) = snd <$> children
children Basic{} = []
children (Repeated _ cs) = cs
children (Sequenced cs _) = snd <$> cs

data SimpleRepr = SimpleRepr Tag Range deriving (Show, Eq)
data Tag = NodeTag String
         | IdentifierTag
         | StringTag
         | IntegerTag
         | FloatTag
         | RepeatTag Repeat
         | SequencedTag
         deriving (Show, Eq)

instance Ranged SimpleRepr where
  range (SimpleRepr _ r) = r

{-
Should essentially be:
Map Range (Set MidNode)
remove items from the set that are children of other items from the set
remove pairs where the set is a singleton
remove ranges that are entirely contained in another range
report each range as ambiguous
-}
