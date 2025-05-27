module Tokenizer (Token(..),
                  isInteger,
                  isWord,
                  tokenizer,
                  test_Tokenizer) where

import Parser
import Data.Char
import Control.Applicative
import Test.HUnit

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
tokenizer = do ts <- many (spaces >> token)
               spaces
               eof
               n <- numParsed
               return $ ts ++ [(n,TokenEnd)]

spaces :: Parser Char ()
spaces = do _ <- many $ pProp isSpace
            return ()

token :: Parser Char (Int,Token)
token = do n <- numParsed
           t <- integer <|> word <|> plus <|> minus <|> times <|> divide <|>
                power <|> openParen <|> closeParen <|> assign
           return (n,t)

integer :: Parser Char Token
integer = do ds <- some $ pProp isDigit
             return $ TokenInteger (read ds :: Integer)

word :: Parser Char Token
word = do c <- pProp isAlpha
          cs <- many $ pProp isAlphaNum
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

test_Tokenizer :: Test
test_Tokenizer = test [
  isLeft (parseAll tokenizer "{") ~?= True,
  isLeft (parseAll tokenizer "abc 123 +-*/ @") ~?= True,
  parseAll tokenizer ""        ~?= Right [(0,TokenEnd)],
  parseAll tokenizer "1"       ~?= Right [(0,TokenInteger 1),(1,TokenEnd)],
  parseAll tokenizer "123"     ~?= Right [(0,TokenInteger 123),(3,TokenEnd)],
  parseAll tokenizer " 1"      ~?= Right [(1,TokenInteger 1),(2,TokenEnd)],
  parseAll tokenizer "   456"  ~?= Right [(3,TokenInteger 456),(6,TokenEnd)],
  parseAll tokenizer " 1 2 3 " ~?= Right [(1,TokenInteger 1),
                                          (3,TokenInteger 2),
                                          (5,TokenInteger 3),
                                          (7,TokenEnd)],
  parseAll tokenizer "    345    5678   34    " ~?=
  Right [(4,TokenInteger 345),
         (11,TokenInteger 5678),
         (18,TokenInteger 34),
         (24,TokenEnd)],
  parseAll tokenizer "-"       ~?= Right [(0,TokenMinus),(1,TokenEnd)],
  parseAll tokenizer " - 9 "   ~?= Right [(1,TokenMinus),
                                          (3,TokenInteger 9),
                                          (5,TokenEnd)],
  parseAll tokenizer "---"     ~?=
  Right [(0,TokenMinus),(1,TokenMinus),(2,TokenMinus),(3,TokenEnd)],
  parseAll tokenizer "abc"     ~?= Right [(0,TokenWord "abc"),(3,TokenEnd)],
  parseAll tokenizer "ABC"     ~?= Right [(0,TokenWord "ABC"),(3,TokenEnd)],
  parseAll tokenizer "a23"     ~?= Right [(0,TokenWord "a23"),(3,TokenEnd)],
  parseAll tokenizer " a11 11" ~?=
  Right [(1,TokenWord "a11"),(5,TokenInteger 11),(7,TokenEnd)],
  parseAll tokenizer ":="      ~?= Right [(0,TokenAssign),(2,TokenEnd)],
  parseAll tokenizer "3-:=-3"  ~?=
  Right [(0,TokenInteger 3),(1,TokenMinus),(2,TokenAssign),
         (4,TokenMinus),(5,TokenInteger 3),(6,TokenEnd)],
  parseAll tokenizer "+"       ~?= Right [(0,TokenPlus),(1,TokenEnd)],
  parseAll tokenizer "("       ~?= Right [(0,TokenOpenParen),(1,TokenEnd)],
  parseAll tokenizer ")"       ~?= Right [(0,TokenCloseParen),(1,TokenEnd)],
  parseAll tokenizer "*"       ~?= Right [(0,TokenTimes),(1,TokenEnd)],
  parseAll tokenizer "/"       ~?= Right [(0,TokenDivide),(1,TokenEnd)],
  parseAll tokenizer "^"       ~?= Right [(0,TokenPower),(1,TokenEnd)]
  ]
  where
    isLeft :: Either a b -> Bool
    isLeft (Left _) = True
    isLeft (Right _) = False
