{-# LANGUAGE RecursiveDo, Rank2Types, ScopedTypeVariables #-}

module BootParser (parseConstructions) where

import Control.Applicative ((<|>), some, many)

import Text.Earley

import Lexer (tokenize)
import Types.Lexer (sameContent, Token(..))
import Types.Construction hiding (implementation)

type Production r a = Prod r String Token a

parseConstructions :: (forall r. Production r (Splice n) -> Grammar r (Production r n)) -> [Token] -> ([[Construction n]], Report String [Token])
parseConstructions impl = fullParses $ parser constructions
  where
    constructions = implementation impl >>= fmap many . construction

implementation :: (Production r (Splice n) -> Grammar r (Production r n)) -> Grammar r (Production r (Maybe (Splice n)))
implementation impl = mdo
  impl' <- impl splice
  let common = lit "(" *> inParens <* lit ")"
           <|> Syntax <$> impl'
           <|> terminal fold <*> identifier <*>
               acc <*> identifier <*
               lit "(" <*> inParens <* lit ")" <*>
               inParens
           <|> terminal fold1 <*> identifier <*>
               acc <*> identifier <*
               lit "(" <*> inParens <* lit ")"
  inParens <- rule $ common <|> Simple <$> identifier
  splice <- rule $ lit "(" *> inParens <* lit ")"
  rule $ Just <$> common <|> simpleOrBuiltin <$> identifier
  where
    fold (IdentifierTok _ "foldl") = Just $ Fold FoldLeft
    fold (IdentifierTok _ "foldr") = Just $ Fold FoldRight
    fold _ = Nothing
    fold1 (IdentifierTok _ "foldl1") = Just $ Fold1 FoldLeft
    fold1 (IdentifierTok _ "foldr1") = Just $ Fold1 FoldRight
    fold1 _ = Nothing
    simpleOrBuiltin "builtin" = Nothing
    simpleOrBuiltin i = Just $ Simple i
    acc = FoldFull <$> identifier
      <|> lit "(" *> pure FoldDestructure <*> commaIds <* lit ")"

construction :: Production r (Maybe (Splice n)) -> Grammar r (Production r (Construction n))
construction impl = mdo
  syntaxPs <- syntaxPatterns
  extraSpec <- rule $ lit "#" *> lit "assoc" *> terminal assoc
                  <|> lit "#" *> lit "prec" *> pure prec <*> integer
                  <|> lit "#" *> lit "bind" *> pure before <*> commaIds <* lit "before"
                  <|> lit "#" *> lit "bind" *> pure after <*> commaIds <* lit "after"
                  <|> lit "#" *> lit "bind" *> pure bind <*> binding
                  <|> lit "#" *> lit "scope" *> pure (bind . ([],)) <*> commaIds
  binding <- rule $ (,) <$> commaIds <* lit "in"
                        <* lit "scope" <*> commaIds
  rule $ lit "syntax" *> pure Construction <*>
         identifier <* lit ":" <*>
         identifier <* lit "=" <*>
         syntaxPs <* lit "{" <*>
         (mconcat <$> many extraSpec) <*>
         impl <* lit "}"
  where
    bind bs = mempty { bindingData = [bs] }
    before bs = mempty { beforeBindings = bs }
    after bs = mempty { afterBindings = bs }
    prec i = mempty { precData = Just i }
    assoc (IdentifierTok _ "left") = Just $ mempty { assocData = Just AssocLeft }
    assoc (IdentifierTok _ "right") = Just $ mempty { assocData = Just AssocRight }
    assoc _ = Nothing

syntaxPatterns :: Grammar r (Prod r String Token [SyntaxPattern])
syntaxPatterns = mdo
  unnamed <- rule $ toSimplePat <$> identifier
                <|> toToken <$> string
                <|> lit "(" *> pure SequencePat <*> some (named <|> repeated) <* lit ")"
  repeated <- rule $ RepeatPat <$> unnamed <*> terminal repeat
                 <|> unnamed
  named <- rule $ NamedPat <$> identifier <* lit ":" <*> repeated
  return . some $ named <|> repeated
  where
    toToken str = case tokenize str of
      [tok] -> TokenPat tok
      toks -> error $ "Parse error: expected single token literal, got: " ++ show toks
    repeat (SymbolTok _ "*") = Just StarRep
    repeat (SymbolTok _ "+") = Just PlusRep
    repeat (SymbolTok _ "?") = Just QuestionRep
    repeat _ = Nothing
    toSimplePat "Identifier" = IdentifierPat
    toSimplePat "Integer" = IntegerPat
    toSimplePat "Float" = FloatPat
    toSimplePat "String" = StringPat
    toSimplePat ty = SyntaxPat ty

commaIds :: Production r [String]
commaIds = (:) <$> identifier <*> many (lit "," *> identifier)

identifier :: Prod r String Token String
identifier = terminal test <?> "identifier"
  where
    test (IdentifierTok _ ident) = Just ident
    test _ = Nothing

string :: Prod r String Token String
string = terminal test <?> "string"
  where
    test (StringTok _ str) = Just str
    test _ = Nothing

integer :: Prod r String Token Int
integer = terminal test <?> "integer"
  where
    test (IntegerTok _ i) = Just i
    test _ = Nothing

lit :: String -> Prod r String Token Token
lit str = satisfy (sameContent patternTok) <?> str
  where
    patternTok = case tokenize str of
      [tok] -> tok
      _ -> error "compiler error: expected single token"

tok :: String -> Token
tok str = case tokenize str of
  [t] -> t
  _ -> error "compiler error: expected single token"
