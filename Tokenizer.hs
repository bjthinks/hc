module Tokenizer (Token(..), isInteger, isWord, tokenizer) where

import Parser
import Data.Char

data Token = TokenInteger Integer |
             TokenWord String |
             TokenMinus |
             TokenAssign
             deriving (Eq, Show)

isInteger :: Token -> Bool
isInteger (TokenInteger _) = True
isInteger _ = False

isWord :: Token -> Bool
isWord (TokenWord _) = True
isWord _ = False

tokenizer :: Parser Char [(Int,Token)]
tokenizer = do ts <- pStar (spaces >> token)
               spaces
               return ts

spaces :: Parser Char ()
spaces = do pStar $ pProp isSpace
            return ()

token :: Parser Char (Int,Token)
token = do n <- numParsed
           t <- integer ||| word ||| minus ||| assign
           return (n,t)

integer :: Parser Char Token
integer = do ds <- pPlus $ pProp isDigit
             return $ TokenInteger (read ds :: Integer)

word :: Parser Char Token
word = do c <- pProp isAlpha
          cs <- pStar $ pProp isAlphaNum
          return $ TokenWord (c:cs)

minus :: Parser Char Token
minus = do pElt '-'
           return TokenMinus

assign :: Parser Char Token
assign = do pElt ':'
            pElt '='
            return TokenAssign