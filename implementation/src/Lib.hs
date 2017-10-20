{-# LANGUAGE RecursiveDo #-}

module Lib (tokenize, parseSyntax, parseGrammar, ambiguities) where

import Lexer (tokenize)
import BootParser (parseSyntax)
import GrammarGenerator (parseGrammar)
import Ambiguity (ambiguities)
