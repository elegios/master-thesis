module Main where

import Lib (tokenize, parseSyntax)

import Data.Char (generalCategory)

main :: IO ()
main = interact $ show . parseSyntax . tokenize

printTokens :: IO ()
printTokens = interact $ unlines . map show . tokenize

checkCategory :: IO ()
checkCategory = interact $ unlines . map (show . generalCategory)
