module Lib
--    ( someFunc
--    , getTokens
--    )
where

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as H
import Data.List (find, intercalate)
import Production

someFunc :: IO ()
someFunc = putStrLn "someFunc"

isTerm :: Symbol -> Bool
isTerm (T _) = True
isTerm _     = False

isNonTerm :: Symbol -> Bool
isNonTerm (NT _) = True
isNonTerm _      = False

tokenNames :: H.HashMap String String
tokenNames = H.fromList [("(","LeftParen")
                         ,(")","RightParen")
                         ,("[","LeftBracket")
                         ,("]","RightBracket")
                         ,("{","LeftBrace")
                         ,("}","RightBrace")
                         ,("+","Plus")
                         ,("-","Minus")
                         ,("*","Times")
                         ,("/","Divide")
                         ,("#","Sharp")
                         ,(",","Comma")
                         ,(".","Period")
                         ]

getTokens :: [Production] -> [String]
getTokens plist = aux plist S.empty
   where aux [] tokenSet = S.toList tokenSet
         aux ((Production _ rhs _):ps) tokenSet =
           aux ps $ foldl (\set (T elt) -> S.insert elt set) tokenSet (filter isTerm rhs)

tokenName :: String -> String
tokenName t =
  case H.lookup t tokenNames of
    Just v -> "Token" ++ v
    Nothing -> "Token_" ++ t

getNonterms :: [Production] -> [String]
getNonterms plist = S.toList $ foldl (\set (Production nt _ _) -> S.insert nt set) S.empty plist

genTokenDecl :: [String] -> String
genTokenDecl tokens =
  header ++
  concatMap (\t -> "    '" ++ t ++ "'   { " ++ tokenName t ++ " }\n") tokens
  where header = "%token\n"

isKLabel :: Attribute -> Bool
isKLabel (KLabel _) = True
isKLabel _          = False

getKLabel (Production nt _ attributes) =
  case find isKLabel attributes of
    Just (KLabel kl) -> kl
    Nothing -> nt

showSymbol :: Symbol -> String
showSymbol (NT s) = s
showSymbol (T s) = "'" ++ s ++ "'"

genProduction p@(Production l symbols attributes) =
  let kl = getKLabel p
   in l ++ ": " ++ intercalate " " (map showSymbol symbols)
