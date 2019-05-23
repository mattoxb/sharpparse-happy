module Main where

import Lib
import Grammar

main :: IO ()
main = do
   c <- getContents
   let k = (kgrammar . lexer) c
   print k
   print "Tokens:"
   let tokens = getTokens k
   putStrLn (genTokenDecl tokens)
   print "Nonterminals:"
   putStrLn (show $ getNonterms k)
   mapM putStrLn (map genProduction k)
   return ()
