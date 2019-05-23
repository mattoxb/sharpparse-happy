module Main where

import Lib
import Grammar

main :: IO ()
main = do
   c <- getContents
   let k = (kgrammar . lexer) c
   print k
   print "Tokens:"
   print (getTokens k)
   return ()
