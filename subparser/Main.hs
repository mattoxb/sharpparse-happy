module Main where

import Lib
import Grammar2

main :: IO ()
main = getContents >>= print . kinkgrammar . lexer
