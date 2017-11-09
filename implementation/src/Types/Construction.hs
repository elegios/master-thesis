{-# LANGUAGE EmptyDataDecls #-}

module Types.Construction where

import Control.Applicative ((<|>))

import Types.Lexer (Token)

data Construction n = Construction
  { name :: String
  , syntaxType :: String
  , syntax :: [SyntaxPattern]
  , extraData :: ExtraData
  , implementation :: Maybe (Splice n) }
  deriving (Show, Eq)

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
                   deriving (Show, Eq)

data Repeat = StarRep | PlusRep | QuestionRep deriving (Show, Eq)

data ExtraData = ExtraData
  { assocData :: Maybe AssocData
  , precData :: Maybe Int
  , beforeBindings :: [String]
  , afterBindings :: [String]
  , bindingData :: [([String], [String])] }
  deriving (Show, Eq)

instance Monoid ExtraData where
  mempty = ExtraData Nothing Nothing [] [] []
  mappend (ExtraData a1 p1 bb1 ba1 bm1) (ExtraData a2 p2 bb2 ba2 bm2) =
    ExtraData (a1 <|> a2)
              (p1 <|> p2)
              (mappend bb1 bb2)
              (mappend ba1 ba2)
              (mappend bm1 bm2)

data AssocData = AssocLeft | AssocRight deriving (Show, Eq)

data FoldDir = FoldLeft | FoldRight deriving (Eq, Show)
data FoldAcc = FoldFull String | FoldDestructure [String] deriving (Show, Eq)

data NoSplice n
data Splice n = Syntax n
              | Simple String
              | Fold FoldDir String FoldAcc String (Splice n) (Splice n)
              | Fold1 FoldDir String FoldAcc String (Splice n)
              deriving (Show, Eq)

instance Show (NoSplice n) where
  show _ = "NoSplice"
instance Eq (NoSplice n) where
  _ == _ = undefined
