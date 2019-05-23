{
module Grammar2 where
import Data.Char
}

%name kinkgrammar
%tokentype { Token }
%error { parseError }

%token

    '+'   { TokenPlus }
    '.'   { TokenPeriod }
    's'   { Token_s }
    '('   { TokenLeftParen }
    ')'   { TokenRightParen }


%%

Nat: Nat '+' Nat  { concat ["plus { } (", $1,",",$3, ")"] } 
Nat: 's' '(' Nat ')'  { concat ["succ { } (", $3, ")"] } 
Nat: '.'  { concat ["zero { } (", "", ")"] } 

{
parseError :: [Token] -> a
parseError x = error ("Parse error: " ++ show x)


data Token = TokenId String
           | TokenPlus
           | TokenPeriod
           | Token_s
           | TokenLeftParen
           | TokenRightParen
   deriving (Show)


lexId [] = ("",[])
lexId (c:xx) | isAlpha c =
  let (result,rest) = span isAlphaNum xx
   in (c:result, rest)

eatws (c:xs) | isSpace c = eatws xs
eatws xx = xx

lexer :: String -> [Token]
lexer [] = []
lexer (c:xs)  | isSpace c = lexer xs
lexer ('+':xs) = TokenPlus: lexer xs
lexer ('.':xs) = TokenPeriod: lexer xs
lexer ('(':xs) = TokenLeftParen: lexer xs
lexer (')':xs) = TokenRightParen: lexer xs

lexer xx = case lexId xx of
    ("",xs) -> [] -- handles eof
    ("s",xs) -> Token_s : lexer (eatws xs)
    ("END",xs) -> [] -- handles eof
    (s,xs) -> TokenId s : lexer xs
}

