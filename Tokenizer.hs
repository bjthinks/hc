module Tokenizer (Token, tokenizer) where

import Parser
import Data.Char

data Token = TokenInteger Integer | TokenMinus deriving Show

tokenizer :: Parser Char [Token]
tokenizer = do ts <- pStar (spaces >> token)
               spaces
               return ts

spaces :: Parser Char ()
spaces = do pStar $ pProp isSpace
            return ()

token :: Parser Char Token
token = integer ||| minus

integer :: Parser Char Token
integer = do ds <- digits
             return $ TokenInteger (read ds :: Integer)

digits :: Parser Char [Char]
digits = pPlus $ pProp isDigit

minus :: Parser Char Token
minus = do pElt '-'
           return TokenMinus