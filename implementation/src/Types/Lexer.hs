module Types.Lexer where

import Data.Data (Data, Typeable)

{- Tokens -}

data Token = IdentifierTok Range String
           | IntegerTok Range Int
           | FloatTok Range Double
           | SymbolTok Range String
           | StringTok Range String
           deriving (Eq, Ord, Data, Typeable)

sameContent :: Token -> Token -> Bool
sameContent (IdentifierTok _ t1) (IdentifierTok _ t2) = t1 == t2
sameContent (IntegerTok _ t1) (IntegerTok _ t2) = t1 == t2
sameContent (FloatTok _ t1) (FloatTok _ t2) = t1 == t2
sameContent (SymbolTok _ t1) (SymbolTok _ t2) = t1 == t2
sameContent (StringTok _ t1) (StringTok _ t2) = t1 == t2
sameContent _ _ = False

instance Show Token where
  show (IdentifierTok _ s) = "identifier(" ++ s ++ ")"
  show (IntegerTok _ i) = "integer(" ++ show i ++ ")"
  show (FloatTok _ d) = "float(" ++ show d ++ ")"
  show (SymbolTok _ s) = "symbol(" ++ s ++ ")"
  show (StringTok _ s) = "string(" ++ s ++ ")"

instance Ranged Token where
  range (IdentifierTok r _) = r
  range (IntegerTok r _) = r
  range (FloatTok r _) = r
  range (SymbolTok r _) = r
  range (StringTok r _) = r

{- Positions and Ranges -}

data Position = Pos { absolute :: !Int
                    , line :: !Int
                    , column :: !Int }
                    deriving (Eq, Show, Data, Typeable)

instance Ord Position where
  compare (Pos a _ _) (Pos b _ _) = compare a b

data Range = Range { start :: !Position, end :: !Position }
           | NoRange
           deriving (Eq, Data, Typeable)

instance Show Range where
  show Range{start, end} = show (line start) ++ ":" ++ show (column start) ++ "-" ++ show (line end) ++ ":" ++ show (column end)
  show NoRange = "(nowhere)"

instance Ord Range where
  compare (Range s1 e1) (Range s2 e2) = compare s1 s2 `mappend` compare e1 e2
  compare NoRange NoRange = EQ
  compare NoRange Range{} = LT
  compare Range{} NoRange = GT

instance Monoid Range where
  mempty = NoRange
  mappend NoRange b = b
  mappend a NoRange = a
  mappend (Range s1 e1) (Range s2 e2) = Range (min s1 s2) (max e1 e2)

class Ranged a where
  range :: a -> Range

instance Ranged Range where
  range = id

instance Ranged a => Ranged [a] where
  range = mconcat . fmap range
