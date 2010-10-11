module Tokenizer (Token(..), isInteger, tokenizer) where

import Parser
import Data.Char

data Token = TokenInteger Integer | TokenMinus deriving (Eq, Show)

isInteger :: Token -> Bool
isInteger (TokenInteger _) = True
isInteger _ = False

tokenizer :: Parser Char [(Int,Token)]
tokenizer = do ts <- pStar (spaces >> token)
               spaces
               return ts

spaces :: Parser Char ()
spaces = do pStar $ pProp isSpace
            return ()

token :: Parser Char (Int,Token)
token = do n <- numParsed
           t <- integer ||| minus
           return (n,t)

integer :: Parser Char Token
integer = do ds <- digits
             return $ TokenInteger (read ds :: Integer)

digits :: Parser Char [Char]
digits = pPlus $ pProp isDigit

minus :: Parser Char Token
minus = do pElt '-'
           return TokenMinus
