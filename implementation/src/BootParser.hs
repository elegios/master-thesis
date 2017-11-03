{-# LANGUAGE RecursiveDo #-}

module BootParser (parseSyntax) where

import Control.Applicative ((<|>), some, many)

import Text.Earley

import Lexer (tokenize)
import Types.Lexer (sameContent, Token(..))
import Types.Construction

parseSyntax :: [Token] -> ([[Construction]], Report String [Token])
parseSyntax = fullParses $ parser constructions
  where
    constructions = many <$> construction

construction :: Grammar r (Prod r String Token Construction)
construction = mdo
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
         (mconcat <$> many extraSpec) <* lit "}"
  where
    commaIds = (:) <$> identifier <*> many (lit "," *> identifier)
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
