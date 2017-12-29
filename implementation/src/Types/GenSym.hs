module Types.GenSym where

data GenSym = GenSym String Int deriving (Eq, Ord)

instance Show GenSym where
  show (GenSym s i) = s ++ "#" ++ show i

expandGenSym :: String -> GenSym
expandGenSym symbol = GenSym symbol (-1)
