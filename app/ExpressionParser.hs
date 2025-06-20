module ExpressionParser (expressionParser, test_ExpressionParser) where

import Parser
import Tokenizer
import Expression
import Data.Either
import Control.Applicative
import Test.HUnit
import Data.Ratio

expressionParser :: Parser Token Expression
expressionParser = additive

additive :: Parser Token Expression
additive = do a <- multiplicative
              as <- many (do op <- match TokenPlus <|> match TokenMinus
                             rhs <- multiplicative
                             return (op,rhs))
              return $ makeAdditive ((TokenPlus,a):as)
  where
    makeAdditive :: [(Token, Expression)] -> Expression
    makeAdditive as = eSum $ map handleSign as
    handleSign :: (Token, Expression) -> Expression
    handleSign (TokenMinus, a) = eProd [eRat (-1), a]
    handleSign (TokenPlus, a) = a
    handleSign _ = undefined

multiplicative :: Parser Token Expression
multiplicative = do a <- unary
                    as <- many (do op <- match TokenTimes <|> match TokenDivide
                                   rhs <- unary
                                   return (op,rhs))
                    return $ makeMultiplicative ((TokenTimes,a):as)
  where
    makeMultiplicative :: [(Token, Expression)] -> Expression
    makeMultiplicative as = eProd $ map handleOperator as
    handleOperator :: (Token, Expression) -> Expression
    handleOperator (TokenTimes, a) = a
    handleOperator (TokenDivide, a) = eIntPow a (-1)
    handleOperator _ = undefined

unary :: Parser Token Expression
unary = do sign <- option $ match TokenMinus
           a <- implicitMult
           return $ case sign of
             Nothing -> a
             Just _ -> eProd [eRat (-1), a]

implicitMult :: Parser Token Expression
implicitMult = do as <- some power
                  return $ eProd as

power :: Parser Token Expression
power = do b <- atom
           e <- option (do _ <- match TokenPower
                           exponent')
           return $ case e of
             Nothing -> b
             Just ee -> eIntPow b ee

exponent' :: Parser Token Integer
exponent' = do sign <- option $ match TokenMinus
               TokenInteger a <- matching isInteger
               return $ case sign of
                 Nothing -> a
                 Just _ -> (-a)

atom :: Parser Token Expression
atom = integer <|> call <|> variable <|> paren

call :: Parser Token Expression
call = callNoArgs <|> callArgs

callNoArgs :: Parser Token Expression
callNoArgs = do TokenWord func <- matching isWord
                _ <- match TokenOpenParen
                _ <- match TokenCloseParen
                return (eCall func [])

callArgs :: Parser Token Expression
callArgs = do TokenWord func <- matching isWord
              _ <- match TokenOpenParen
              a <- additive
              as <- many $ match TokenComma >> additive
              _ <- match TokenCloseParen
              return $ eCall func (a:as)

paren :: Parser Token Expression
paren = do _ <- match TokenOpenParen
           a <- additive
           _ <- match TokenCloseParen
           return a

integer :: Parser Token Expression
integer = do TokenInteger n <- matching isInteger
             return $ eRat $ n % 1

variable :: Parser Token Expression
variable = do TokenWord w <- matching isWord
              return $ eVar w

tRat :: Rational -> Expression
tVar :: String -> Expression
tSum :: [Expression] -> Expression
tProd :: [Expression] -> Expression
tIntPow :: Expression -> Integer -> Expression
tCall :: String -> [Expression] -> Expression
(tRat,tVar,tSum,tProd,tIntPow,tCall) = unsafeExpressionConstructors

test_ExpressionParser :: [Test]
test_ExpressionParser =
  [ parseAll variable [TokenWord "abc"] ~?= Right (tVar "abc")
  , parseAll variable [TokenWord "Aa1"] ~?= Right (tVar "Aa1")
  , isLeft (parseAll variable [TokenInteger 7]) ~?= True
  , isLeft (parseAll variable [TokenPlus]) ~?= True
  , isLeft (parseAll variable [TokenMinus]) ~?= True
  , isLeft (parseAll variable [TokenTimes]) ~?= True
  , isLeft (parseAll variable [TokenDivide]) ~?= True
  , isLeft (parseAll variable [TokenPower]) ~?= True
  , isLeft (parseAll variable [TokenOpenParen]) ~?= True
  , isLeft (parseAll variable [TokenCloseParen]) ~?= True
  , isLeft (parseAll variable [TokenAssign]) ~?= True
  , isLeft (parseAll variable [TokenEnd]) ~?= True

  , isLeft (parseAll integer [TokenWord "abc"]) ~?= True
  , parseAll integer [TokenInteger 7] ~?= Right (tRat (7 % 1))
  , parseAll integer [TokenInteger (345)] ~?= Right (tRat (345 % 1))
  , isLeft (parseAll integer [TokenPlus]) ~?= True
  , isLeft (parseAll integer [TokenMinus]) ~?= True
  , isLeft (parseAll integer [TokenTimes]) ~?= True
  , isLeft (parseAll integer [TokenDivide]) ~?= True
  , isLeft (parseAll integer [TokenPower]) ~?= True
  , isLeft (parseAll integer [TokenOpenParen]) ~?= True
  , isLeft (parseAll integer [TokenCloseParen]) ~?= True
  , isLeft (parseAll integer [TokenAssign]) ~?= True
  , isLeft (parseAll integer [TokenEnd]) ~?= True

  , parseAll multiplicative [TokenInteger 1,TokenTimes,TokenInteger 2,
                             TokenTimes, TokenInteger 3] ~?=
    Right (tRat (6 % 1))
  , parseAll multiplicative [TokenInteger 1,TokenDivide,TokenInteger 2,
                           TokenDivide, TokenInteger 3] ~?= Right (tRat (1 % 6))
  , parseAll multiplicative
    [TokenInteger 1,TokenDivide,TokenInteger 2,TokenTimes,TokenInteger 3,
     TokenDivide,TokenInteger 4] ~?= Right (tRat (3 % 8))
  , parseAll multiplicative
    [TokenMinus,TokenInteger 1,TokenDivide,TokenMinus,TokenInteger 2,TokenTimes,
     TokenInteger 3,TokenDivide,TokenMinus,TokenInteger 4] ~?=
    Right (tRat (-3 % 8))
  , parseAll additive [TokenInteger 1,TokenPlus,TokenInteger 2,
                       TokenPlus,TokenInteger 3] ~?= Right (tRat (6 % 1))
  , parseAll additive [TokenInteger 1,TokenMinus,TokenInteger 2,
                       TokenMinus,TokenInteger 3] ~?= Right (tRat (-4 % 1))
  , parseAll additive
    [TokenInteger 1,TokenMinus,TokenInteger 2,TokenPlus,TokenInteger 3,
     TokenMinus,TokenInteger 4] ~?= Right (tRat (-2 % 1))
  , parseAll additive
    [TokenMinus,TokenInteger 1,TokenMinus,TokenMinus,TokenInteger 2,TokenPlus,
     TokenInteger 3,TokenMinus,TokenMinus,TokenInteger 4] ~?=
    Right (tRat (8 % 1))
  , parseAll additive [TokenInteger 1,TokenMinus,TokenInteger 2] ~?=
    Right (tRat (-1 % 1))
  , parseAll additive [TokenInteger 1,TokenMinus,TokenMinus,TokenInteger 2] ~?=
    Right (tRat (3 % 1))
  , isLeft (parseAll additive [TokenInteger 1,TokenMinus,TokenMinus,TokenMinus,
                               TokenInteger 2]) ~?= True
  , parseAll additive [TokenMinus,TokenInteger 3] ~?= Right (tRat (-3 % 1))
  , isLeft (parseAll additive [TokenMinus,TokenMinus,TokenInteger 3]) ~?= True
  , parseAll unary [TokenInteger 1,TokenPower,TokenInteger 2] ~?=
    Right (tRat (1 % 1))
  , parseAll unary [TokenMinus,TokenInteger 1,TokenPower,TokenInteger 2] ~?=
    Right (tRat (-1 % 1))
  , parseAll unary [TokenInteger 1,TokenPower,TokenMinus,TokenInteger 2] ~?=
    Right (tRat (1 % 1))
  , parseAll unary [TokenMinus,TokenInteger 1,TokenPower,TokenMinus,
                    TokenInteger 2] ~?=
    Right (tRat (-1 % 1))
  , isLeft (parseAll unary [TokenMinus,TokenMinus,TokenInteger 1,TokenPower,
                            TokenInteger 2]) ~?= True
  , isLeft (parseAll unary [TokenInteger 1,TokenPower,TokenMinus,TokenMinus,
                            TokenInteger 2]) ~?= True
  , isLeft (parseAll unary [TokenInteger 1,TokenPower,TokenInteger 2,TokenPower,
                            TokenInteger 3,TokenPower,TokenInteger 4]) ~?= True
    -- TODO: Exponents involving only constants could be calculated
{-
  , parseAll additive [TokenMinus,TokenOpenParen,TokenInteger 1,TokenPlus,
                       TokenInteger 2,TokenCloseParen,TokenPower,
                       TokenInteger 3] ~?=
    Right (ASTNegation (ASTPower (ASTSum (ASTInteger 1) (ASTInteger 2))
                        (ASTInteger 3)))
  , parseAll additive [TokenWord "a",TokenOpenParen,TokenWord "b",
                       TokenCloseParen] ~?=
    Right (ASTCall "a" [ASTVariable "b"])
  , parseAll additive [TokenWord "a",TokenOpenParen,TokenInteger 1,
                       TokenCloseParen] ~?= Right (ASTCall "a" [ASTInteger 1])
  , parseAll additive [TokenInteger 1,TokenOpenParen,TokenInteger 2,
                       TokenCloseParen] ~?= Right (ASTProduct (ASTInteger 1)
                                                   (ASTInteger 2))
  , parseAll additive [TokenInteger 1,TokenOpenParen,TokenWord "b",
                       TokenCloseParen] ~?= Right (ASTProduct (ASTInteger 1)
                                                   (ASTVariable "b"))
  , parseAll additive [TokenWord "f",TokenOpenParen,TokenCloseParen] ~?=
    Right (ASTCall "f" [])
  , parseAll additive [TokenWord "a",TokenOpenParen,TokenWord "b",TokenComma,
                       TokenWord "c",TokenCloseParen] ~?=
    Right (ASTCall "a" [ASTVariable "b",ASTVariable "c"])
  , parseAll additive [TokenWord "a",TokenPower,TokenOpenParen,TokenWord "b",
                       TokenPlus,TokenWord "c",TokenCloseParen] ~?=
    Right (ASTPower (ASTVariable "a") (ASTSum (ASTVariable "b")
                                       (ASTVariable "c"))) -}
  ]
