{
module Grammar2 where
import Data.Char
}

%name kgrammar
%tokentype { Token }
%error { parseError }

%token
   t        { TokenT $$ }
   nt       { TokenNT $$ }
   id       { TokenId $$ }
   '::='    { TokenDefinedAs }
   'syntax' { TokenSyntax }
   ';'      { TokenSemicolon }
   'klabel' { TokenKLabel }
   '('      { TokenLeftParen }
   ')'      { TokenRightParen }
   '['      { TokenLeftBracket }
   ']'      { TokenRightBracket }

%%

Grm : Productions  { reverse $1 }

Productions : { [] }
            | Production  { [ $1 ] }
            | Productions Production { $2 : $1 }

Production : 'syntax' id '::=' symbols '[' attribute ']' { Production $2 (reverse $4) [$6] }

symbol : id  { NT $1 }
       | t   { T $1 }

symbols : { [] }
        | symbols symbol { $2 : $1 }

attribute : 'klabel' '(' id ')' { KLabel $3 }

{
parseError :: [Token] -> a
parseError x = error ("Parse error: " ++ show x)

data Production = Production String [Symbol] [Attribute]
  deriving Show

data Symbol = NT String | T String
  deriving Show

data Attribute = Function
               | KLabel String
  deriving Show

data Token
      = TokenModule
      | TokenSyntax
      | TokenNT String
      | TokenT String
      | TokenDefinedAs
      | TokenSemicolon
      | TokenLeftBracket
      | TokenRightBracket
      | TokenLeftParen
      | TokenRightParen
      | TokenId String
      | TokenKLabel
      | TokenError String
    deriving Show

lexId [] = ("",[])
lexId (c:xx) | isAlpha c =
  let (result,rest) = span isAlphaNum xx
   in (c:result, rest)

notQuote '"' = False
notQuote _ = True

lexer :: String -> [Token]
lexer [] = []
lexer (c:xs)  | isSpace c = lexer xs
lexer (':':':':'=':xs) = TokenDefinedAs : lexer xs
lexer (';':xs) = TokenSemicolon : lexer xs
lexer ('[':xs) = TokenLeftBracket : lexer xs
lexer (']':xs) = TokenRightBracket : lexer xs
lexer ('(':xs) = TokenLeftParen : lexer xs
lexer (')':xs) = TokenRightParen : lexer xs
lexer ('"':xx) = case span notQuote xx of
      ("",xs) -> parseError [TokenError xs]
      (s,'"':xs) -> TokenT s : lexer xs
lexer xx = case lexId xx of
      ("",xs) -> [] -- handles eof
      ("syntax",xs) -> TokenSyntax : lexer xs
      ("klabel",xs) -> TokenKLabel : lexer xs
      ("END",xs) -> [] -- handles eof
      (s,xs) -> TokenId s : lexer xs

localmain = getContents >>= print . kgrammar . lexer

}
