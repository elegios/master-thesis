{-# LANGUAGE Rank2Types #-}

module GrammarGenerator (parseGrammar, Node(..), MidNode(..)) where

import Data.Function ((&), on)
import Data.List (groupBy, sortBy, intercalate)
import Data.Maybe (mapMaybe, maybeToList)
import Data.Traversable (mapAccumL, mapAccumR)
import Data.Foldable (asum, foldlM)
import Control.Applicative (some, many, optional)
import Control.Arrow ((&&&), first, second)
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
    generateConstruction nonTerminals thisLevel nextLevel Construction{name, syntaxType, syntax, extraData} =
      uncurry (Node name) <$> case assocData extraData of
        Just _ -> snd $ sequencePats OnceThis syntax
        Nothing -> snd $ sequencePats AlwaysThis syntax
      where
        sequencePats acc pats = mapAccum toProd acc pats
          & second (fmap namedAndRange . sequenceA)
        mapAccum = case assocData extraData of
          Just AssocRight -> mapAccumR
          _ -> mapAccumL
        namedAndRange children =
          let r = mconcat $ range . snd <$> children
              named = mapMaybe (\(n, mn) -> (, mn) <$> n) children
          in (named, r)
        toProd acc = \case
          IdentifierPat -> basic acc identifier
          IntegerPat -> basic acc integer
          FloatPat -> basic acc float
          StringPat -> basic acc string
          TokenPat tok -> (acc,) $ (Nothing,) . Basic <$> satisfy (sameContent tok) <?> show tok
          NamedPat n pat -> Just n & const & first & fmap & second $ toProd acc pat
          SequencePat pats -> (Nothing,) . uncurry Sequenced & fmap & second $ sequencePats acc pats
          RepeatPat pat rep -> (Nothing,) . Repeated rep & fmap & second $ case rep of
            StarRep -> second (many . fmap snd) $ toProd acc pat
            PlusRep -> second (some . fmap snd) $ toProd acc pat
            QuestionRep -> second (fmap maybeToList . optional . fmap snd) $ toProd acc pat
          SyntaxPat pat ->
            let (acc', prod) = if pat == currentTy
                then (nextAssoc acc, if this acc then thisLevel else nextLevel)
                else (acc, nonTerminals M.! pat)
            in (acc', (Nothing,) . MidNode <$> prod <?> pat)
        basic acc = (acc,) . fmap ((Nothing,) . Basic)

data AssocState = AlwaysThis | OnceThis | NeverThis deriving (Eq)

nextAssoc :: AssocState -> AssocState
nextAssoc AlwaysThis = AlwaysThis
nextAssoc OnceThis = NeverThis
nextAssoc NeverThis = NeverThis

this :: AssocState -> Bool
this = (/= NeverThis)

integer :: Production r Token
integer = satisfy (\case { IntegerTok{} -> True; _ -> False }) <?> "integer"
float :: Production r Token
float = satisfy (\case { FloatTok{} -> True; _ -> False }) <?> "float"
string :: Production r Token
string = satisfy (\case { StringTok{} -> True; _ -> False }) <?> "string"
identifier :: Production r Token
identifier = satisfy (\case { IdentifierTok{} -> True; _ -> False }) <?> "identifier"

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
