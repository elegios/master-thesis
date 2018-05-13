{
module Lexer (tokenize) where

import Data.Word (Word8)

import Data.Char(ord)
import qualified Data.Bits

import Types.Lexer
}

$digit = [0-9]
$alpha = [a-zA-Z]
$single_symbol = [\;\,\{\}\(\)\[\]`]
$other_symbol = [:\.\*\+\?\#\~\-\$\|\^\/\<\>=]

tokens :-
  $white+                              ;
  "//".*                               ;
  [$alpha \_] [$alpha $digit \_ \']*   { IdentifierTok }
  $digit+                              { \r s -> IntegerTok r (read s) }
  $digit+ \. $digit+                   { \r s -> FloatTok r (read s) }
  $single_symbol                       { SymbolTok }
  $other_symbol+                       { SymbolTok }
  [\"] [^\"]* [\"]                     { \r s -> StringTok r (processString s)}

{

processString :: String -> String
processString = init . tail

tokenize = alexScanTokens

-- Alex stuff, slightly rewritten "posn" wrapper (to have a range, not just a position)


-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

type Byte = Word8

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))

alexStartPos :: Position
alexStartPos = Pos 0 1 1

alexMove :: Position -> Char -> Position
alexMove (Pos a l c) '\t' = Pos (a+1)  l     (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1)
alexMove (Pos a l _) '\n' = Pos (a+1) (l+1)   1
alexMove (Pos a l c) _    = Pos (a+1)  l     (c+1)

type AlexInput = (Position,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- rest of the bytes for the current char
                  String)       -- current input string


--alexScanTokens :: String -> [token]
alexScanTokens str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError ((Pos _ line column),_,_,_) -> error $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp'@(end,_,_,_) len act -> act (Range pos end) (take len str) : go inp'
}
