module Types.Construction where

import Control.Applicative ((<|>))

import Types.Lexer (Token)

data Construction = Construction
  { name :: String
  , syntaxType :: String
  , syntax :: [SyntaxPattern]
  , extraData :: ExtraData }
  deriving (Show)

-- The Strings are the names of the various segments (argument names, so to speak)
data SyntaxPattern = IdentifierPat
                   | TokenPat Token
                   | IntegerPat
                   | FloatPat
                   | StringPat
                   | SyntaxPat String
                   | RepeatPat SyntaxPattern Repeat
                   | SequencePat [SyntaxPattern]
                   | NamedPat String SyntaxPattern
                   deriving (Show)

data Repeat = StarRep | PlusRep | QuestionRep deriving (Show, Eq)

data ExtraData = ExtraData
  { assocData :: Maybe AssocData
  , precData :: Maybe Int
  , beforeBindings :: [String]
  , afterBindings :: [String]
  , bindingData :: [([String], [String])] }
  deriving (Show)

instance Monoid ExtraData where
  mempty = ExtraData Nothing Nothing [] [] []
  mappend (ExtraData a1 p1 bb1 ba1 bm1) (ExtraData a2 p2 bb2 ba2 bm2) =
    ExtraData (a1 <|> a2)
              (p1 <|> p2)
              (mappend bb1 bb2)
              (mappend ba1 ba2)
              (mappend bm1 bm2)

data AssocData = AssocLeft | AssocRight deriving (Show)
