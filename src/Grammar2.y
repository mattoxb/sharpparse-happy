{
module Grammar2 where
import Data.Char
}

%name kinkgrammar
%tokentype { Token }
%error { parseError }

%token

    'foobar'   { Token_foobar }
    'buzz'   { Token_buzz }
    'bar'   { Token_bar }


%%

Foo: 'bar'  { concat ["bar3 { } (", "", ")"] } 
Foo: 'buzz'  { concat ["buzz2 { } (", "", ")"] } 
Fizz: 'foobar'  { concat ["foobar { } (", "", ")"] } 

{
parseError :: [Token] -> a
parseError x = error ("Parse error: " ++ show x)


data Token = TokenId String
           | Token_foobar
           | Token_buzz
           | Token_bar
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

lexer xx = case lexId xx of
    ("",xs) -> [] -- handles eof
    ("foobar",xs) -> Token_foobar : lexer (eatws xs)
    ("buzz",xs) -> Token_buzz : lexer (eatws xs)
    ("bar",xs) -> Token_bar : lexer (eatws xs)
    ("END",xs) -> [] -- handles eof
    (s,xs) -> TokenId s : lexer xs
}

