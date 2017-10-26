{-# LANGUAGE Rank2Types #-}

module GrammarGenerator (parseGrammar) where

import Data.Function ((&), on)
import Data.List (groupBy, sortBy)
import Data.Maybe (mapMaybe, maybeToList)
import Data.Traversable (mapAccumL, mapAccumR)
import Data.Foldable (asum, foldlM)
import Control.Applicative (some, many, optional)
import Control.Arrow ((&&&), first, second)
import Control.Monad.Fix (mfix)

import qualified Data.Map as M

import Text.Earley

import Types.Lexer (sameContent, Token(..), Ranged(..))
import Types.Construction
import Types.Ast

type Production r a = Prod r String Token a

parseGrammar :: String -> [Construction] -> [Token] -> ([Node], Report String [Token])
parseGrammar startTy constructions = fullParses $ parser $ do
  syntax <- mfix $ \nonTerminals ->
    constructions
      & fmap (syntaxType &&& (:[]))
      & M.fromListWith mappend
      & M.traverseWithKey (generateType nonTerminals)
  return $ syntax M.! startTy

generateType :: M.Map String (Production r Node) -> String -> [Construction] -> Grammar r (Production r Node)
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
            in (acc', (Nothing,) . MidNode <$> prod)
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
