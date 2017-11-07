{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FunctionalDependencies #-}

module GrammarGenerator (parseWithGrammar, implementationGrammar) where

import Data.Function ((&), on)
import Data.List (groupBy, sortBy)
import Data.Maybe (mapMaybe, maybeToList)
import Data.Traversable (mapAccumL, mapAccumR)
import Data.Foldable (asum, foldlM)
import Control.Applicative (some, many, optional, (<|>))
import Control.Arrow ((&&&), first, second)
import Control.Monad.Fix (mfix)

import qualified Data.Map as M

import Text.Earley hiding (token)

import Types.Lexer (sameContent, Token(..), Ranged(..), Range)
import Types.Construction
import Types.Ast

type Production r a = Prod r String Token a

type Node = NodeI String
type MidNode = MidNodeI String

parseWithGrammar :: String -> M.Map String (Construction n) -> [Token] -> ([Node], Report String [Token])
parseWithGrammar startTy constructions = fullParses $ parser $ do
  syntax <- mfix $ \nonTerminals ->
    M.elems constructions
      & fmap (syntaxType &&& (:[]))
      & M.fromListWith mappend
      & M.traverseWithKey (generateType nonTerminals)
  return $ syntax M.! startTy

-- TODO: foldl/foldr for implementation
-- TODO: splice a fold expr into raw implementation
implementationGrammar :: M.Map String (Construction n) -> Grammar r (Production r Node)
implementationGrammar constructions = do
  syntax <- mfix $ \nonTerminals ->
    M.elems constructions
      & fmap (syntaxType &&& (:[]))
      & M.fromListWith mappend
      & M.traverseWithKey (generateType nonTerminals)
  rule . asum $ toImpl <$> M.toList syntax
  where
    toImpl (syntaxType, prod) =
      satisfy (sameContent $ IdentifierTok undefined syntaxType) *>
      satisfy (sameContent $ SymbolTok undefined "`") *>
      prod

generateType :: M.Map String (Production r Node) -> String -> [Construction n] -> Grammar r (Production r Node)
generateType nonTerminals currentTy constructions =
  constructions
    & sortBy (compare `on` fmap (* (-1)) . precData . extraData)
    & groupBy ((==) `on` precData . extraData)
    & (\(bot:rest) -> genBot bot >>= \bot' -> foldlM genLevel bot' rest)
  where
    currentType = nonTerminals M.! currentTy
    genBot level =
      generateConstruction nonTerminals currentType currentType <$> level
        & asum & rule
    genLevel nextLevel level = mfix $ \thisLevel ->
      generateConstruction nonTerminals thisLevel nextLevel <$> level
        & (nextLevel:) & asum & rule
    generateConstruction :: M.Map String (Production r Node) -> Production r Node -> Production r Node -> Construction n -> Production r Node
    generateConstruction nonTerminals thisLevel nextLevel Construction{name, syntaxType, syntax, extraData} =
      construct <$> case assocData extraData of
        Just _ -> snd $ sequencePats syntax OnceThis
        Nothing -> snd $ sequencePats syntax AlwaysThis
      where
        construct (r, cs) = Node { name = name, nodeRange = r, children = cs }
        sequencePats pats acc = mapAccum (flip toProd) acc pats
          & second (fmap rangeAndNamed . sequenceA)
        mapAccum = case assocData extraData of
          Just AssocRight -> mapAccumR
          _ -> mapAccumL
        rangeAndNamed children =
          let r = mconcat $ range . snd <$> children
              named = mapMaybe (\(n, mn) -> (, mn) <$> n) children
          in (r, named)
        toProd = \case
          IdentifierPat -> accname $ uncurry MidIdentifier <$> identifier <|> splice
          IntegerPat -> accname $ Basic <$> integer <|> splice
          FloatPat -> accname $ Basic <$> float <|> splice
          StringPat -> accname $ Basic <$> string <|> splice
          TokenPat tok -> accname $ Basic <$> token tok <|> splice
          NamedPat n pat -> setName n . toProd pat
          SequencePat pats -> accname $
            modifyResult (uncurry Sequenced) . sequencePats pats
          RepeatPat pat rep -> accname $
            second (fmap (Repeated rep) . repF rep . fmap snd) . toProd pat
          SyntaxPat pat | pat == currentTy -> accname $
            nextAssoc &&& boolean thisLevel nextLevel . this
          SyntaxPat pat -> accname $ nonTerminals M.! pat
        repF StarRep = many
        repF PlusRep = some
        repF QuestionRep = fmap maybeToList . optional
        setName n = modifyResult . first . const $ Just n

modifyResult :: Applicative f => (a -> b) -> (c, f a) -> (c, f b)
modifyResult f = second $ fmap f

data AssocState = AlwaysThis | OnceThis | NeverThis deriving (Eq)

nextAssoc :: AssocState -> AssocState
nextAssoc AlwaysThis = AlwaysThis
nextAssoc OnceThis = NeverThis
nextAssoc NeverThis = NeverThis

this :: AssocState -> Bool
this = (/= NeverThis)

class AccNamed a r | a -> r where
  accname :: a -> AssocState -> (AssocState, Production r (Maybe String, MidNode))

instance AccNamed (Production r MidNode) r where
  accname prod acc = (acc,) $ (Nothing,) <$> prod

instance AccNamed (Production r Node) r where
  accname prod acc = (acc,) $ (Nothing,) . MidNode <$> prod

instance AccNamed (AssocState -> (AssocState, Production r MidNode)) r where
  accname f acc = modifyResult (Nothing,) $ f acc

instance AccNamed (AssocState -> (AssocState, Production r Node)) r where
  accname f acc = modifyResult ((Nothing,) . MidNode) $ f acc

integer :: Production r Token
integer = satisfy (\case { IntegerTok{} -> True; _ -> False }) <?> "integer"
float :: Production r Token
float = satisfy (\case { FloatTok{} -> True; _ -> False }) <?> "float"
string :: Production r Token
string = satisfy (\case { StringTok{} -> True; _ -> False }) <?> "string"
identifier :: Production r (Range, String)
identifier = terminal (\case { (IdentifierTok r s) -> Just (r, s); _ -> Nothing }) <?> "identifier"
token :: Token -> Production r Token
token tok = satisfy (sameContent tok) <?> show tok
splice :: Production r MidNode
splice = satisfy (sameContent $ SymbolTok undefined "`") *> pure (uncurry SyntaxSplice) <*> identifier

boolean :: a -> a -> Bool -> a
boolean a _ True = a
boolean _ b False = b
