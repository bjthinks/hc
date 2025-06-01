module Factor.SquareFree(squareFree,mIsSquareFree,isGoodModulus,goodModulus) where

import Factor.Defs
import Factor.Polynomial
import Factor.ModularPolynomial

squareFree :: Polynomial -> Factorization Polynomial
squareFree f =
  -- if f has content c, then f' has content ck for some k.
  -- then a0 has content c as well.
  -- b1 has no content
  -- so all bi have no content
  -- so all ai i>0 have no content
  let c = signedContent f
      g = divideBySignedContent f
      g' = derivative g
      a0 = polynomialGcd g g'
      b1 = quotient g a0
      c1 = quotient g' a0
      d1 = c1 - derivative b1
      factorsAndPowers = reverse (yun b1 d1 1)
      in Factorization c factorsAndPowers
  where
    yun (Polynomial [Term _ 0]) _ _ = []
    yun bi di i =
      let ai = polynomialGcd bi di
          bip = quotient bi ai
          cip = quotient di ai
          dip = cip - derivative bip
          rest = yun bip dip (i+1)
      in if isConstant ai then rest else (ai,i) : rest
    isConstant (Polynomial []) = True
    isConstant (Polynomial [Term _ 0]) = True
    isConstant _ = False

mIsSquareFree :: ModularPolynomial -> Bool
mIsSquareFree f = mIsConstantPolynomial $ mPolynomialGcd f $ mDerivative f

reduceMod :: Coeff -> Polynomial -> ModularPolynomial
reduceMod m (Polynomial ts) = makeModularPolynomial m ts

isGoodModulus :: Coeff -> Polynomial -> Bool
isGoodModulus m p =
  let q = reduceMod m p
  in degree p == mDegree q && mIsSquareFree q

-- The argument f must be square free. Finds a modulus so that f is
-- also square free mod m.
goodModulus :: Polynomial -> Coeff
goodModulus p = head $
  filter (flip isGoodModulus p) primes
  where -- TODO Need a real list of primes
    primes = [2,3,5,7,11,13,17,19]
