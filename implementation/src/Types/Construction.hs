{-# LANGUAGE EmptyDataDecls #-}

module Types.Construction where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData, rnf)

import Control.Applicative ((<|>))
import Data.Data (Data, Typeable)

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
                   deriving (Show, Eq, Data, Typeable)

data Repeat = StarRep | PlusRep | QuestionRep deriving (Show, Eq, Data, Typeable, Generic)
instance NFData Repeat

data ExtraData = ExtraData
  { assocData :: Maybe AssocData
  , precData :: Maybe Int
  , beforeBindings :: [String]
  , afterBindings :: [String]
  , bindingData :: [([String], [String])]
  , scopeData :: [ScopeData] }
  deriving (Show, Eq)

data ScopeData = ScopeData (Maybe String) [String] [ScopeData] deriving (Show, Eq)

instance Monoid ExtraData where
  mempty = ExtraData Nothing Nothing [] [] [] []
  mappend (ExtraData a1 p1 bb1 ba1 bm1 s1) (ExtraData a2 p2 bb2 ba2 bm2 s2) =
    ExtraData (a1 <|> a2)
              (p1 <|> p2)
              (mappend bb1 bb2)
              (mappend ba1 ba2)
              (mappend bm1 bm2)
              (mappend s1 s2)

data AssocData = AssocLeft | AssocRight deriving (Show, Eq)

data FoldDir = FoldLeft | FoldRight deriving (Eq, Show)

data NoSplice n
instance NFData (NoSplice n) where
  rnf !_ = ()
data Splice n = Syntax n
              | Simple String
              | Fold FoldDir String String (Splice n) (Splice n)
              | Fold1 FoldDir String String (Splice n)
              deriving (Show, Eq)

instance Show (NoSplice n) where
  show _ = "NoSplice"
instance Eq (NoSplice n) where
  _ == _ = undefined
