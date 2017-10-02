module Main where

import Lib (tokenize)

import Data.Char (generalCategory)

main :: IO ()
main = interact $ unlines . map show . tokenize

checkCategory :: IO ()
checkCategory = interact $ unlines . map (show . generalCategory)
