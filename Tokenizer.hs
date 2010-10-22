module Tokenizer (Token(..), isInteger, isWord, tokenizer) where

import Parser
import Data.Char

data Token = TokenInteger Integer |
             TokenWord String |
             TokenPlus |
             TokenMinus |
             TokenTimes |
             TokenDivide |
             TokenPower |
             TokenOpenParen |
             TokenCloseParen |
             TokenAssign |
             TokenEnd
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
               pEnd
               n <- numParsed
               return $ ts ++ [(n,TokenEnd)]

spaces :: Parser Char ()
spaces = do pStar $ pProp isSpace
            return ()

token :: Parser Char (Int,Token)
token = do n <- numParsed
           t <- integer ||| word ||| plus ||| minus ||| times ||| divide |||
                power ||| openParen ||| closeParen ||| assign
           return (n,t)

integer :: Parser Char Token
integer = do ds <- pPlus $ pProp isDigit
             return $ TokenInteger (read ds :: Integer)

word :: Parser Char Token
word = do c <- pProp isAlpha
          cs <- pStar $ pProp isAlphaNum
          return $ TokenWord (c:cs)

plus   :: Parser Char Token
minus  :: Parser Char Token
times  :: Parser Char Token
divide :: Parser Char Token
power  :: Parser Char Token
openParen  :: Parser Char Token
closeParen :: Parser Char Token
assign :: Parser Char Token

plus   = pElt '+' >> return TokenPlus
minus  = pElt '-' >> return TokenMinus
times  = pElt '*' >> return TokenTimes
divide = pElt '/' >> return TokenDivide
power  = pElt '^' >> return TokenPower
openParen  = pElt '(' >> return TokenOpenParen
closeParen = pElt ')' >> return TokenCloseParen
assign = pElt ':' >> pElt '=' >> return TokenAssign
