{-# LANGUAGE RecursiveDo #-}

module Lib (tokenize, parseSyntax, parseGrammar) where

import Lexer (tokenize)
import BootParser (parseSyntax)
import GrammarGenerator (parseGrammar)

