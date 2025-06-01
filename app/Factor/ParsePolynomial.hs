module Factor.ParsePolynomial(parsePolynomial, parseModularPolynomial) where

import Text.Parsec
import Control.Applicative (some)
import Factor.Defs
import Factor.Polynomial
import Factor.ModularPolynomial

type MyParser = Parsec String ()

infixl 3 |||
(|||) :: MyParser a -> MyParser a -> MyParser a
(|||) lhs rhs = try lhs <|> rhs

parsePolynomial :: String -> Polynomial
parsePolynomial str =
  let result = parse pPolynomial "" str
  in case result of
    Left err -> error $ show err
    Right poly -> poly

parseModularPolynomial :: String -> ModularPolynomial
parseModularPolynomial str =
  let result = parse pModularPolynomial "" str
  in case result of
    Left err -> error $ show err
    Right mpoly -> mpoly

pSign :: MyParser Char
pSign = do
  s <- oneOf "+-"
  spaces
  return s

pCoeff :: MyParser Coeff
pCoeff = do
  number <- some digit
  spaces
  return (read number :: Coeff)

pMaybeCoeff :: MyParser Coeff
pMaybeCoeff = pCoeff ||| return 1

pX :: MyParser ()
pX = do
  _ <- char 'x'
  spaces
  return ()

pCarat :: MyParser ()
pCarat = do
  _ <- char '^'
  spaces
  return ()

pExponent :: MyParser Exponent
pExponent = do
  number <- some digit
  spaces
  return (read number :: Exponent)

pGeneralTerm :: MyParser Term
pGeneralTerm = do
  c <- pMaybeCoeff
  pX
  pCarat
  e <- pExponent
  return $ Term c e

pLinearTerm :: MyParser Term
pLinearTerm = do
  c <- pMaybeCoeff
  pX
  return $ Term c 1

pConstantTerm :: MyParser Term
pConstantTerm = do
  c <- pCoeff
  return $ Term c 0

pTerm :: MyParser Term
pTerm = pGeneralTerm ||| pLinearTerm ||| pConstantTerm

pSignedTerm :: MyParser Term
pSignedTerm = do
  s <- pSign
  Term c e <- pTerm
  let sc = if s == '+' then c else -c
  return $ Term sc e

pLeadingTerm :: MyParser Term
pLeadingTerm = pSignedTerm ||| pTerm

pTerms :: MyParser [Term]
pTerms = many pSignedTerm

pPolynomial :: MyParser Polynomial
pPolynomial = do
  spaces
  t <- pLeadingTerm
  ts <- pTerms
  eof
  return $ makePolynomial (t:ts)

pMod :: MyParser ()
pMod = do
  _ <- string "mod"
  spaces
  return ()

pModularPolynomial :: MyParser ModularPolynomial
pModularPolynomial = do
  spaces
  t <- pLeadingTerm
  ts <- pTerms
  pMod
  m <- pCoeff
  eof
  return $ makeModularPolynomial m (t:ts)
