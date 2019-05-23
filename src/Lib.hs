module Lib
--    ( someFunc
--    , getTokens
--    )
where

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as H
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
                         ]

getTokens plist = aux plist S.empty
   where aux [] tokenSet = tokenSet
         aux ((Production _ rhs _):ps) tokenSet =
           aux ps $ foldl (\set (T elt) -> S.insert elt set) tokenSet (filter isTerm rhs)
