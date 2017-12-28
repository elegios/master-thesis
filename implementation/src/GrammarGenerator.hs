{-# LANGUAGE Rank2Types, ScopedTypeVariables, FlexibleContexts #-}

module GrammarGenerator (parseWithGrammar, implementationGrammar) where

import Data.Function ((&), on)
import Data.List (groupBy, sortBy)
import Data.Maybe (maybeToList)
import Data.Traversable (mapAccumL, mapAccumR)
import Data.Foldable (asum, foldlM)
import Control.Applicative (some, many, optional, empty, (<|>))
import Control.Arrow ((&&&), second)
import Control.Monad.Fix (mfix)

import qualified Data.Map as M

import Text.Earley hiding (token)

import Types.Lexer (sameContent, Token(..), Ranged(..), Range)
import Types.Construction
import Types.Ast

type Production r a = Prod r String Token a

type Node s = NodeI s String
type MidNode s = MidNodeI s String
type SplicedNode = FixNode Splice String

parseWithGrammar :: String -> M.Map String (Construction n) -> [Token] -> ([FixNode NoSplice String], Report String [Token])
parseWithGrammar startTy constructions = fullParses $ parser $ do
  syntax <- mfix $ \nonTerminals ->
    M.elems constructions
      & fmap (syntaxType &&& (:[]))
      & M.fromListWith mappend
      & M.traverseWithKey (generateType empty nonTerminals)
  return $ FixNode <$> syntax M.! startTy

implementationGrammar :: M.Map String (Construction n) -> Production r (Splice SplicedNode) -> Grammar r (Production r SplicedNode)
implementationGrammar constructions splice = do
  syntax <- mfix $ \nonTerminals ->
    M.elems constructions
      & fmap (syntaxType &&& (:[]))
      & M.fromListWith mappend
      & M.traverseWithKey (generateType splice nonTerminals)
  rule . fmap FixNode . asum $ toImpl <$> M.toList syntax
  where
    toImpl (syntaxType, prod) =
      token (IdentifierTok undefined syntaxType) *>
      token (SymbolTok undefined "`") *>
      prod

-- TODO: cleanup, potentially in relation to AccNamed
generateType :: forall r s n. Production r s -> M.Map String (Production r (Node s)) -> String -> [Construction n] -> Grammar r (Production r (Node s))
generateType splice nonTerminals currentTy constructions =
  constructions
    & sortBy (compare `on` fmap (* (-1)) . precData . extraData)
    & groupBy ((==) `on` precData . extraData)
    & (\(bot:rest) -> genBot bot >>= \bot' -> foldlM genLevel bot' rest)
  where
    typedSplice :: (s -> b s String) -> String -> Production r (b s String)
    typedSplice constr ty =
      token (SymbolTok undefined "`") *>
      token (IdentifierTok undefined ty) *>
      pure constr <*> splice
    currentType = nonTerminals M.! currentTy
    genBot level =
      generateConstruction currentType currentType <$> level
        & asum & (<|> typedSplice SyntaxSplice "t") & rule
    genLevel nextLevel level = mfix $ \thisLevel ->
      generateConstruction thisLevel nextLevel <$> level
        & (nextLevel:) & asum & rule
    generateConstruction thisLevel nextLevel Construction{name, syntax, extraData} =
      construct <$> case assocData extraData of
        Just _ -> snd $ sequencePats syntax OnceThis
        Nothing -> snd $ sequencePats syntax AlwaysThis
      where
        construct (r, cs) = Node { name = name, nodeRange = r, children = cs }
        sequencePats pats acc = mapAccum (flip toProd) acc pats
          & second (fmap (range &&& id) . sequenceA)
        mapAccum = case assocData extraData of
          Just AssocRight -> mapAccumR
          _ -> mapAccumL
        toProd = \case
          IdentifierPat -> accname $ uncurry MidIdentifier <$> identifier <|> typedSplice MidSplice "id"
          IntegerPat -> accname $ integer <|> typedSplice MidSplice "int"
          FloatPat -> accname $ float <|> typedSplice MidSplice "float"
          StringPat -> accname $ string <|> typedSplice MidSplice "str"
          TokenPat tok -> accname $ token tok
          NamedPat _ pat -> toProd pat
          SequencePat pats -> accname $
            modifyResult (uncurry Sequenced) . sequencePats pats
          RepeatPat pat rep -> accname $
            second (fmap (Repeated rep) . repF rep) . toProd pat
          SyntaxPat pat | pat == currentTy -> accname $
            nextAssoc &&& boolean thisLevel nextLevel . this
          SyntaxPat pat -> accname $ nonTerminals M.! pat
        repF StarRep = many
        repF PlusRep = some
        repF QuestionRep = fmap maybeToList . optional

modifyResult :: Applicative f => (a -> b) -> (c, f a) -> (c, f b)
modifyResult f = second $ fmap f

data AssocState = AlwaysThis | OnceThis | NeverThis deriving (Eq)

nextAssoc :: AssocState -> AssocState
nextAssoc AlwaysThis = AlwaysThis
nextAssoc OnceThis = NeverThis
nextAssoc NeverThis = NeverThis

this :: AssocState -> Bool
this = (/= NeverThis)

class AccNamed a r s | a -> r s where
  accname :: a -> AssocState -> (AssocState, Production r (MidNode s))

instance AccNamed (Production r (MidNode s)) r s where
  accname prod acc = (acc,) prod

instance AccNamed (Production r (Node s)) r s where
  accname prod acc = (acc,) $ MidNode <$> prod

instance AccNamed (AssocState -> (AssocState, Production r (MidNode s))) r s where
  accname f acc = f acc

instance AccNamed (AssocState -> (AssocState, Production r (Node s))) r s where
  accname f acc = modifyResult MidNode $ f acc

integer :: Production r (MidNode s)
integer = Basic <$> satisfy (\case { IntegerTok{} -> True; _ -> False }) <?> "integer"
float :: Production r (MidNode s)
float = Basic <$> satisfy (\case { FloatTok{} -> True; _ -> False }) <?> "float"
string :: Production r (MidNode s)
string = Basic <$> satisfy (\case { StringTok{} -> True; _ -> False }) <?> "string"
identifier :: Production r (Range, String)
identifier = terminal (\case { (IdentifierTok r s) -> Just (r, s); _ -> Nothing }) <?> "identifier"
token :: Token -> Production r (MidNode s)
token tok = Basic <$> satisfy (sameContent tok) <?> show tok

boolean :: a -> a -> Bool -> a
boolean a _ True = a
boolean _ b False = b
