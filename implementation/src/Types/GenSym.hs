module Types.GenSym where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data GenSym = GenSym String Int deriving (Eq, Ord, Generic)
instance NFData GenSym

instance Show GenSym where
  show (GenSym s i) = s ++ "#" ++ show i

expandGenSym :: String -> GenSym
expandGenSym symbol = GenSym symbol (-1)
