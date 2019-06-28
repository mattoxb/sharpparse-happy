{
module Grammar2 where
import Data.Char
}

%name kinkgrammar
%tokentype { Token }
%error { parseError }

%token
   id       { TokenId $$ }
   '('      { TokenLeftParen }
   ')'      { TokenRightParen }
   '['      { TokenLeftBracket }
   ']'      { TokenRightBracket }
   's'      { Token_s }
   '+'      { TokenPlus }
   '.'      { TokenPeriod }
%%

Grm : Nat  { $1 }

Nat: Nat '+' Nat      { "plus { } (" ++ $1 ++ "," ++ $3 ++ ")" } 
   | 's' '(' Nat ')'  { "succ { } (" ++ $3 ++ ")" } 
   | '.'              { "zero { } ( )" }

{
parseError :: [Token] -> a
parseError x = error ("Parse error: " ++ show x)

data Token
      = Token_s
      | TokenPlus
      | TokenLeftBracket
      | TokenRightBracket
      | TokenLeftParen
      | TokenRightParen
      | TokenId String
      | TokenPeriod
      | TokenError String
    deriving Show

lexId [] = ("",[])
lexId (c:xx) | isAlpha c =
  let (result,rest) = span isAlphaNum xx
   in (c:result, rest)

notQuote '"' = False
notQuote _ = True

eatws (c:xs) | isSpace c = eatws xs
eatws xx = xx

lexer :: String -> [Token]
lexer [] = []
lexer (c:xs)  | isSpace c = lexer xs
lexer ('.':xs) = TokenPeriod : lexer xs
lexer ('+':xs) = TokenPlus : lexer xs
lexer ('(':xs) = TokenLeftParen : lexer xs
lexer (')':xs) = TokenRightParen : lexer xs
lexer xx = case lexId xx of
      ("",xs) -> [] -- handles eof
      ("s",xs) -> Token_s : lexer (eatws xs)
      ("END",xs) -> [] -- handles eof
      (s,xs) -> TokenId s : lexer xs
}
