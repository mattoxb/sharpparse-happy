module Main where

import Lib
import Grammar

main :: IO ()
main = do
   c <- getContents
   let k = (kgrammar . lexer) c
   let tokens = getTokens k

   -- Print out header
   putStrLn happyHeader

   -- Print out tokens
   putStrLn (genTokenDecl tokens)
   putStrLn "\n%%\n"

   -- Print out grammar
   mapM putStrLn (map genProduction k)
   putStrLn ""

   -- Print out lexer
   putStrLn happyMedium
   putStrLn ""

   -- Print out token data type
   putStrLn $ genTokenData tokens

   -- Print out the lexer
   putStrLn lexerBegin
   putStrLn $ lexSymbols tokens
   putStrLn "lexer xx = case lexId xx of"
   putStrLn "    (\"\",xs) -> [] -- handles eof"
   putStr   $ lexTerminals tokens
   putStrLn "    (\"END\",xs) -> [] -- handles eof"
   putStrLn "    (s,xs) -> TokenId s : lexer xs"
   putStrLn "}\n"
   return ()
