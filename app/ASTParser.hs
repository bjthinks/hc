module ASTParser (astParser,test_ASTParser) where

import Parser
import Tokenizer
import AST
import Control.Applicative
import Test.HUnit

astParser :: Parser Token AST
astParser = additive

additive :: Parser Token AST
additive = do a <- multiplicative
              as <- many (do op <- match TokenPlus <|> match TokenMinus
                             rhs <- multiplicative
                             return (op,rhs))
              return $ makeAdditive a as
  where
    makeAdditive :: AST -> [(Token,AST)] -> AST
    makeAdditive a [] = a
    makeAdditive a ((TokenPlus,b):bs) = makeAdditive (ASTSum a b) bs
    makeAdditive a ((TokenMinus,b):bs) = makeAdditive (ASTDifference a b) bs
    makeAdditive _ _ = undefined

multiplicative :: Parser Token AST
multiplicative = do a <- unary
                    as <- many (do op <- match TokenTimes <|> match TokenDivide
                                   rhs <- unary
                                   return (op,rhs))
                    return $ makeMultiplicative a as
  where
    makeMultiplicative :: AST -> [(Token,AST)] -> AST
    makeMultiplicative a [] = a
    makeMultiplicative a ((TokenTimes,b):bs) =
      makeMultiplicative (ASTProduct a b) bs
    makeMultiplicative a ((TokenDivide,b):bs) =
      makeMultiplicative (ASTQuotient a b) bs
    makeMultiplicative _ _ = undefined

unary :: Parser Token AST
unary = do sign <- option $ match TokenMinus
           a <- implicitMult
           return $ case sign of
             Nothing -> a
             Just _ -> ASTNegation a

implicitMult :: Parser Token AST
implicitMult = do a <- power
                  as <- many power
                  return $ makeMultiplicative a as
  where
    makeMultiplicative :: AST -> [AST] -> AST
    makeMultiplicative a [] = a
    makeMultiplicative a (b:bs) =
      makeMultiplicative (ASTProduct a b) bs

power :: Parser Token AST
power = do b <- atom
           e <- option (do _ <- match TokenPower
                           exponent')
           return $ case e of
             Nothing -> b
             Just ee -> ASTPower b ee

exponent' :: Parser Token AST
exponent' = do sign <- option $ match TokenMinus
               a <- power
               return $ case sign of
                 Nothing -> a
                 Just _ -> ASTNegation a

atom :: Parser Token AST
atom = integer <|> call <|> variable <|> paren

call :: Parser Token AST
call = callNoArgs <|> callArgs

callNoArgs :: Parser Token AST
callNoArgs = do TokenWord func <- matching isWord
                _ <- match TokenOpenParen
                _ <- match TokenCloseParen
                return (ASTCall func [])

callArgs :: Parser Token AST
callArgs = do TokenWord func <- matching isWord
              _ <- match TokenOpenParen
              a <- additive
              as <- many $ match TokenComma >> additive
              _ <- match TokenCloseParen
              return $ ASTCall func (a:as)

paren :: Parser Token AST
paren = do _ <- match TokenOpenParen
           e <- additive
           _ <- match TokenCloseParen
           return e

integer :: Parser Token AST
integer = do TokenInteger n <- matching isInteger
             return $ ASTInteger n

variable :: Parser Token AST
variable = do TokenWord w <- matching isWord
              return $ ASTVariable w


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

test_ASTParser :: [Test]
test_ASTParser = [
  parseAll variable [TokenWord "abc"] ~?= Right (ASTVariable "abc"),
  parseAll variable [TokenWord "Aa1"] ~?= Right (ASTVariable "Aa1"),
  isLeft (parseAll variable [TokenInteger 7]) ~?= True,
  isLeft (parseAll variable [TokenPlus]) ~?= True,
  isLeft (parseAll variable [TokenMinus]) ~?= True,
  isLeft (parseAll variable [TokenTimes]) ~?= True,
  isLeft (parseAll variable [TokenDivide]) ~?= True,
  isLeft (parseAll variable [TokenPower]) ~?= True,
  isLeft (parseAll variable [TokenOpenParen]) ~?= True,
  isLeft (parseAll variable [TokenCloseParen]) ~?= True,
  isLeft (parseAll variable [TokenAssign]) ~?= True,
  isLeft (parseAll variable [TokenEnd]) ~?= True,

  isLeft (parseAll integer [TokenWord "abc"]) ~?= True,
  parseAll integer [TokenInteger 7] ~?= Right (ASTInteger 7),
  parseAll integer [TokenInteger (345)] ~?= Right (ASTInteger 345),
  isLeft (parseAll integer [TokenPlus]) ~?= True,
  isLeft (parseAll integer [TokenMinus]) ~?= True,
  isLeft (parseAll integer [TokenTimes]) ~?= True,
  isLeft (parseAll integer [TokenDivide]) ~?= True,
  isLeft (parseAll integer [TokenPower]) ~?= True,
  isLeft (parseAll integer [TokenOpenParen]) ~?= True,
  isLeft (parseAll integer [TokenCloseParen]) ~?= True,
  isLeft (parseAll integer [TokenAssign]) ~?= True,
  isLeft (parseAll integer [TokenEnd]) ~?= True,

  parseAll multiplicative [TokenInteger 1,TokenTimes,TokenInteger 2,
                           TokenTimes, TokenInteger 3] ~?=
  Right (ASTProduct (ASTProduct (ASTInteger 1) (ASTInteger 2)) (ASTInteger 3)),
  parseAll multiplicative [TokenInteger 1,TokenDivide,TokenInteger 2,
                           TokenDivide, TokenInteger 3] ~?=
  Right (ASTQuotient (ASTQuotient (ASTInteger 1) (ASTInteger 2))
         (ASTInteger 3)),
  parseAll multiplicative
  [TokenInteger 1,TokenDivide,TokenInteger 2,TokenTimes,TokenInteger 3,
   TokenDivide,TokenInteger 4] ~?=
  Right (ASTQuotient (ASTProduct (ASTQuotient (ASTInteger 1) (ASTInteger 2))
                      (ASTInteger 3)) (ASTInteger 4)),
  parseAll multiplicative
  [TokenMinus,TokenInteger 1,TokenDivide,TokenMinus,TokenInteger 2,TokenTimes,
   TokenInteger 3,TokenDivide,TokenMinus,TokenInteger 4] ~?=
  Right (ASTQuotient (ASTProduct (ASTQuotient (ASTNegation (ASTInteger 1))
                                  (ASTNegation (ASTInteger 2)))
                      (ASTInteger 3)) (ASTNegation (ASTInteger 4))),

  parseAll additive [TokenInteger 1,TokenPlus,TokenInteger 2,
                     TokenPlus,TokenInteger 3] ~?=
  Right (ASTSum (ASTSum (ASTInteger 1) (ASTInteger 2)) (ASTInteger 3)),
  parseAll additive [TokenInteger 1,TokenMinus,TokenInteger 2,
                     TokenMinus,TokenInteger 3] ~?=
  Right (ASTDifference (ASTDifference (ASTInteger 1) (ASTInteger 2))
         (ASTInteger 3)),
  parseAll additive
  [TokenInteger 1,TokenMinus,TokenInteger 2,TokenPlus,TokenInteger 3,
   TokenMinus,TokenInteger 4] ~?=
  Right (ASTDifference (ASTSum (ASTDifference (ASTInteger 1) (ASTInteger 2))
                      (ASTInteger 3)) (ASTInteger 4)),
  parseAll additive
  [TokenMinus,TokenInteger 1,TokenMinus,TokenMinus,TokenInteger 2,TokenPlus,
   TokenInteger 3,TokenMinus,TokenMinus,TokenInteger 4] ~?=
  Right (ASTDifference (ASTSum (ASTDifference (ASTNegation (ASTInteger 1))
                                (ASTNegation (ASTInteger 2)))
                        (ASTInteger 3)) (ASTNegation (ASTInteger 4))),
  parseAll additive [TokenInteger 1,TokenMinus,TokenInteger 2] ~?=
  Right (ASTDifference (ASTInteger 1) (ASTInteger 2)),
  parseAll additive [TokenInteger 1,TokenMinus,TokenMinus,TokenInteger 2] ~?=
  Right (ASTDifference (ASTInteger 1) (ASTNegation (ASTInteger 2))),
  isLeft (parseAll additive [TokenInteger 1,TokenMinus,TokenMinus,TokenMinus,
                             TokenInteger 2]) ~?= True,
  parseAll additive [TokenMinus,TokenInteger 3] ~?=
  Right (ASTNegation (ASTInteger 3)),
  isLeft (parseAll additive [TokenMinus,TokenMinus,TokenInteger 3]) ~?= True,

  parseAll unary [TokenInteger 1,TokenPower,TokenInteger 2] ~?=
  Right (ASTPower (ASTInteger 1) (ASTInteger 2)),
  parseAll unary [TokenMinus,TokenInteger 1,TokenPower,TokenInteger 2] ~?=
  Right (ASTNegation (ASTPower (ASTInteger 1) (ASTInteger 2))),
  parseAll unary [TokenInteger 1,TokenPower,TokenMinus,TokenInteger 2] ~?=
  Right (ASTPower (ASTInteger 1) (ASTNegation (ASTInteger 2))),
  parseAll unary [TokenMinus,TokenInteger 1,TokenPower,TokenMinus,
                  TokenInteger 2] ~?=
  Right (ASTNegation (ASTPower (ASTInteger 1) (ASTNegation (ASTInteger 2)))),
  isLeft (parseAll unary [TokenMinus,TokenMinus,TokenInteger 1,TokenPower,
                          TokenInteger 2]) ~?= True,
  isLeft (parseAll unary [TokenInteger 1,TokenPower,TokenMinus,TokenMinus,
                          TokenInteger 2]) ~?= True,
  parseAll unary [TokenInteger 1,TokenPower,TokenInteger 2,TokenPower,
                  TokenInteger 3,TokenPower,TokenInteger 4] ~?=
  Right (ASTPower (ASTInteger 1)
         (ASTPower (ASTInteger 2)
          (ASTPower (ASTInteger 3)
           (ASTInteger 4)))),

  parseAll additive [TokenMinus,TokenOpenParen,TokenInteger 1,TokenPlus,
                     TokenInteger 2,TokenCloseParen,TokenPower,
                     TokenInteger 3] ~?=
  Right (ASTNegation (ASTPower (ASTSum (ASTInteger 1) (ASTInteger 2))
                      (ASTInteger 3))),

  parseAll additive [TokenWord "a",TokenOpenParen,TokenWord "b",
                     TokenCloseParen] ~?=
  Right (ASTCall "a" [ASTVariable "b"]),
  parseAll additive [TokenWord "a",TokenOpenParen,TokenInteger 1,
                     TokenCloseParen] ~?= Right (ASTCall "a" [ASTInteger 1]),
  parseAll additive [TokenInteger 1,TokenOpenParen,TokenInteger 2,
                     TokenCloseParen] ~?= Right (ASTProduct (ASTInteger 1)
                                                 (ASTInteger 2)),
  parseAll additive [TokenInteger 1,TokenOpenParen,TokenWord "b",
                     TokenCloseParen] ~?= Right (ASTProduct (ASTInteger 1)
                                                 (ASTVariable "b")),
  parseAll additive [TokenWord "f",TokenOpenParen,TokenCloseParen] ~?=
    Right (ASTCall "f" []),
  parseAll additive [TokenWord "a",TokenOpenParen,TokenWord "b",TokenComma,
                     TokenWord "c",TokenCloseParen] ~?=
    Right (ASTCall "a" [ASTVariable "b",ASTVariable "c"]),
  parseAll additive [TokenWord "a",TokenPower,TokenOpenParen,TokenWord "b",
                     TokenPlus,TokenWord "c",TokenCloseParen] ~?=
    Right (ASTPower (ASTVariable "a") (ASTSum (ASTVariable "b")
                                      (ASTVariable "c")))
  ]
