module Ambiguity (ambiguities, Repr) where

import Data.List (transpose, nub)
import Data.Char (isAlphaNum)
import Control.Arrow ((&&&))

import Types.Lexer (Ranged(..), Range)
import Types.Construction (Repeat(..))
import Types.Ast (NodeI(Node, name, SyntaxSplice), MidNodeI(..), FixNode(..))

type Node s = FixNode s String
type MidNode s = MidNodeI s String

ambiguities :: [Node s] -> [(Range, [Repr])]
ambiguities nodes = ambig $ wrap <$> nodes
  where
    r = range nodes
    wrap (FixNode n) = midWrap $ MidNode n
    midWrap m = MidNode $ Node "*wrap*" [m] (range m)

-- invariant: all items in the argument list share a SimpleRepr
ambig :: [MidNode s] -> [(Range, [Repr])]
ambig [] = []
ambig nodes@(node:_) =
  let cs = children <$> nodes
      childLengthsEqual = allEq $ length <$> cs
      simpleCs = fmap toSimple <$> cs
      similarColumns = allEq <$> transpose simpleCs
      nonSimilar = keepOnly (not <$> similarColumns) <$> simpleCs
      similar = keepOnly similarColumns <$> cs
      conflictingReprs = case filter not similarColumns of
        [] -> []
        [_] -> toRepr . head . keepOnly (not <$> similarColumns) <$> cs
        _ -> toRepr <$> nodes
  in case node of
       Repeated{} | not childLengthsEqual -> return . singleton $ toRepr <$> nodes
       _ | and similarColumns -> concat $ ambig <$> transpose similar
       _ -> singleton conflictingReprs : concat (ambig <$> transpose similar)
  where
    singleton = range &&& nub

toSimple :: MidNode s -> SimpleRepr
toSimple m = SimpleRepr (toTag m) (range m)

toRepr :: MidNode s -> Repr
toRepr m =
  Repr (toTag m)
       (filter (isInteresting . fst) $ (toTag &&& range) <$> children m)
       (range m)
  where
    isInteresting = (/= BasicTag)

toTag :: MidNode s -> Tag
toTag (MidNode Node{name}) = NodeTag name
toTag (MidNode SyntaxSplice{}) = SpliceTag
toTag MidIdentifier{} = IdentifierTag
toTag (MidSplice _) = SpliceTag
toTag Basic{} = BasicTag
toTag (Repeated rep _) = (RepeatTag rep)
toTag Sequenced{} = SequencedTag

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (a:as) = all (a ==) as

keepOnly :: [Bool] -> [a] -> [a]
keepOnly (True:bs) (a:as) = a : keepOnly bs as
keepOnly (False:bs) (_:as) = keepOnly bs as
keepOnly _ _ = []

children :: MidNode s -> [MidNode s]
children (MidNode (Node _ children _)) = children
children (MidNode (SyntaxSplice _)) = []
children MidIdentifier{} = []
children MidSplice{} = []
children Basic{} = []
children (Repeated _ cs) = cs
children (Sequenced _ cs) = cs

data Repr = Repr Tag [(Tag, Range)] Range deriving (Show, Eq)

data SimpleRepr = SimpleRepr Tag Range deriving (Show, Eq)
data Tag = NodeTag String
         | IdentifierTag
         | SpliceTag
         | BasicTag
         | RepeatTag Repeat
         | SequencedTag
         deriving (Eq)

instance Ranged Repr where
  range (Repr _ _ r) = r
instance Ranged SimpleRepr where
  range (SimpleRepr _ r) = r

instance Show Tag where
  show (NodeTag name) = case break (== '#') name of
    (name, []) -> name
    ([], name) -> name
    (p : prefix, name) -> p : formatPrefix prefix ++ name
    where
      formatPrefix [] = ""
      formatPrefix (s : n : rest)
        | not $ isAlphaNum s = s : n : formatPrefix rest
      formatPrefix (_ : rest) = formatPrefix rest
  show IdentifierTag{} = "identifier"
  show SpliceTag{} = "splice"
  show BasicTag{} = "token"
  show (RepeatTag StarRep) = "rep*"
  show (RepeatTag PlusRep) = "rep+"
  show (RepeatTag QuestionRep) = "rep?"
  show SequencedTag{} = "sequence"
