{-# LANGUAGE Rank2Types #-}

module GrammarGenerator (parseGrammar, Node(..), MidNode(..)) where

import Data.Function ((&), on)
import Data.List (groupBy, sortBy, intercalate)
import Data.Maybe (mapMaybe, maybeToList)
import Data.Foldable (asum, foldlM)
import Control.Applicative (some, many, optional)
import Control.Arrow ((&&&))
import Control.Monad.Fix (mfix)
import qualified Data.Map as M

import Text.Earley

import Lexer (sameContent, Range, Token(..), Ranged(..))
import BootParser

data Node = Node
  { name :: String
  , children :: [(String, MidNode)]
  , nodeRange :: Range }

data MidNode = MidNode Node
             | Basic Token
             | Repeated Repeat [MidNode]
             | Sequenced [(String, MidNode)] Range

type Production r a = Prod r String Token a

parseGrammar :: String -> [Construction] -> [Token] -> ([Node], Report String [Token])
parseGrammar startTy constructions = fullParses $ parser $ do
  syntax <- mfix $ \nonTerminals ->
    constructions
      & groupBy ((==) `on` syntaxType)
      & fmap (syntaxType . head &&& id)
      & mapM (\p@(ty, _) -> (ty,) <$> generateType p nonTerminals)
      & fmap M.fromList
  return $ syntax M.! startTy

generateType :: (String, [Construction]) -> M.Map String (Production r Node) -> Grammar r (Production r Node)
generateType (currentTy, constructions) nonTerminals =
  constructions
    & groupBy ((==) `on` precData . extraData)
    & sortBy (compare `on` fmap (* (-1)) . precData . extraData . head)
    & (\(bot:rest) -> genBot bot >>= \bot' -> foldlM genLevel bot' rest)
  where
    currentType = nonTerminals M.! currentTy
    genBot level =
      generateConstruction nonTerminals currentType currentType <$> level
        & asum & rule
    genLevel nextLevel level = mfix $ \thisLevel ->
      generateConstruction nonTerminals thisLevel nextLevel <$> level
        & (nextLevel:) & asum & rule
    generateConstruction :: M.Map String (Production r Node) -> Production r Node -> Production r Node -> Construction -> Production r Node
    generateConstruction nonTerminals thisLevel nextLevel Construction{name, syntaxType, syntax} = -- TODO: assoc
      uncurry (Node name) <$> sequencePats syntax
      where
        toProd (NamedPat n pat) = (Just n,) <$> toProd' pat
        toProd a = (Nothing,) <$> toProd' a
        toProd' (NamedPat _ pat) = toProd' pat
        toProd' IdentifierPat = identifier
        toProd' (TokenPat tok) = Basic <$> satisfy (sameContent tok) <?> show tok
        toProd' IntegerPat = integer
        toProd' FloatPat = float
        toProd' StringPat = string
        toProd' (SyntaxPat p)
          | p == currentTy = MidNode <$> thisLevel <?> p -- TODO: assoc
          | otherwise = MidNode <$> nonTerminals M.! p <?> p
        toProd' (RepeatPat pat rep) = Repeated rep <$> case rep of
          StarRep -> many $ toProd' pat
          PlusRep -> some $ toProd' pat
          QuestionRep -> fmap maybeToList . optional $ toProd' pat
        toProd' (SequencePat pats) = uncurry Sequenced <$> sequencePats pats
        sequencePats pats = toProd <$> pats
          & sequenceA
          & fmap namedAndRange
        namedAndRange children =
          let r = mconcat $ range . snd <$> children
              named = mapMaybe (\(n, mn) -> (, mn) <$> n) children
          in (named, r)

integer :: Production r MidNode
integer = Basic <$> satisfy (\case { IntegerTok{} -> True; _ -> False }) <?> "integer"
float :: Production r MidNode
float = Basic <$> satisfy (\case { FloatTok{} -> True; _ -> False }) <?> "float"
string :: Production r MidNode
string = Basic <$> satisfy (\case { StringTok{} -> True; _ -> False }) <?> "string"
identifier :: Production r MidNode
identifier = Basic <$> satisfy (\case { IdentifierTok{} -> True; _ -> False }) <?> "identifier"

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
