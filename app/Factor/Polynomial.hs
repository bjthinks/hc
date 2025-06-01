module Factor.Polynomial where

import Data.List
import Factor.Defs

newtype Polynomial = Polynomial [Term]
  deriving (Eq, Show, Read)

printPolynomial :: Polynomial -> String
printPolynomial (Polynomial []) = "0"
printPolynomial (Polynomial [t]) = prettyPrintTerm t
  where
    prettyPrintTerm (Term c 0) = show c
    prettyPrintTerm (Term 1 e) = prettyPrintPower e
    prettyPrintTerm (Term (-1) e) = "-" ++ prettyPrintPower e
    prettyPrintTerm (Term c e) = show c ++ prettyPrintPower e
    prettyPrintPower 1 = "x"
    prettyPrintPower e = "x^" ++ show e
printPolynomial (Polynomial (t:ts)) =
  printPolynomial (Polynomial [t]) ++
  addSignAndSpaces (printPolynomial (Polynomial ts))
  where
    addSignAndSpaces ('-':str) = " - " ++ str
    addSignAndSpaces str = " + " ++ str

makePolynomial :: [Term] -> Polynomial
makePolynomial = Polynomial . eliminateZeros . addLikeTerms . sortTerms
  where
    eliminateZeros :: [Term] -> [Term]
    eliminateZeros [] = []
    eliminateZeros (Term 0 _ : ts) = eliminateZeros ts
    eliminateZeros (t:ts) = t : eliminateZeros ts
    addLikeTerms :: [Term] -> [Term]
    addLikeTerms [] = []
    addLikeTerms [t] = [t]
    addLikeTerms (t1@(Term c1 e1):t2s@((Term c2 e2):ts))
      | e1 == e2 = addLikeTerms ((Term (c1+c2) e1):ts)
      | otherwise = t1 : addLikeTerms t2s
    sortTerms :: [Term] -> [Term]
    sortTerms = reverse . sortOn termExponent -- TODO

multiplyTermByTerm :: Term -> Term -> Term
multiplyTermByTerm (Term c1 e1) (Term c2 e2) = Term (c1*c2) (e1+e2)

multiplyTermByList :: Term -> [Term] -> [Term]
multiplyTermByList t = map $ multiplyTermByTerm t

multiplyTermByPolynomial :: Term -> Polynomial -> Polynomial
multiplyTermByPolynomial t (Polynomial ts) =
  Polynomial (multiplyTermByList t ts)

multiplyConstantByPolynomial :: Coeff -> Polynomial -> Polynomial
multiplyConstantByPolynomial c p = multiplyTermByPolynomial (Term c 0) p

instance Num Polynomial where
  Polynomial xs + Polynomial ys = Polynomial $ addTerms xs ys
    where
      addTerms ps [] = ps
      addTerms [] qs = qs
      addTerms allps@(t1@(Term c1 e1):ps) allqs@(t2@(Term c2 e2):qs)
        | e1 > e2 = t1 : addTerms ps allqs
        | e1 < e2 = t2 : addTerms allps qs
        | c1+c2 == 0 = addTerms ps qs
        | otherwise = Term (c1+c2) e1 : addTerms ps qs
  -- * could be made more efficient
  -- for instance, join together like terms before collecting all possible
  -- cross terms into one giant list...
  Polynomial xs * Polynomial ys = makePolynomial $ multiplyTerms xs ys
    where
      multiplyTerms [] _ = []
      multiplyTerms (p:ps) qs = multiplyTermByList p qs ++
                                multiplyTerms ps qs
  negate (Polynomial xs) = Polynomial $ negateTerms xs
    where
      negateTerms [] = []
      negateTerms ((Term c e):ts) = Term (-c) e : negateTerms ts
  abs _ = error "No abs for Polynomial"
  signum _ = error "No signum for Polynomial"
  fromInteger c = makePolynomial [Term( fromInteger c) 0]

derivative :: Polynomial -> Polynomial
derivative (Polynomial ts) = Polynomial (applyDiff ts)
  where
    applyDiff [] = []
    applyDiff [Term _ 0] = []
    applyDiff (Term c e : us) = Term (c*e) (e-1) : applyDiff us

leadingTerm :: Polynomial -> Term
leadingTerm (Polynomial []) = error "zero polynomial has no leading term"
leadingTerm (Polynomial ts) = head ts

leadingCoeff :: Polynomial -> Coeff
leadingCoeff = termCoeff . leadingTerm

degree :: Polynomial -> Exponent
degree = termExponent . leadingTerm

constantPolynomial :: Coeff -> Polynomial
constantPolynomial 0 = Polynomial []
constantPolynomial c = Polynomial [Term c 0]

-- divisionStep dividend divisor = (c, q, r)
-- so that q is the leading term of the quotient (without a denominator c)
-- and c * dividend = divisor * q + r
divisionStep :: Polynomial -> Polynomial -> (Coeff, Term, Polynomial)
divisionStep _ (Polynomial []) = error "attempt to divide a polynomial by 0"
divisionStep (Polynomial []) _ = error "no terms to divide"
divisionStep dividend divisor =
  let Term dividendLeadingCoeff dividendDegree = leadingTerm dividend
      Term divisorLeadingCoeff divisorDegree = leadingTerm divisor
      gcdOfLeadingCoeffs = gcd dividendLeadingCoeff divisorLeadingCoeff
      quoti = Term (dividendLeadingCoeff `div` gcdOfLeadingCoeffs)
              (dividendDegree - divisorDegree)
      constant = divisorLeadingCoeff `div` gcdOfLeadingCoeffs
      remain = multiplyTermByPolynomial (Term constant 0) dividend -
               multiplyTermByPolynomial quoti divisor
  in (constant, quoti, remain)

-- divide dividend divisor = (constant, quotient, remainder)
divide :: Polynomial -> Polynomial -> (Coeff, Polynomial, Polynomial)
divide startingDividend divisor =
  let (c, q, r) = divide' startingDividend
  in (c, Polynomial q, r)
  where
    divide' :: Polynomial -> (Coeff, [Term], Polynomial)
    divide' dividend
      | divisor == Polynomial [] = error
        "attempt to divide a polynomial by zero"
      | dividend == Polynomial [] = (1, [], Polynomial [])
      | degree dividend < degree divisor = (1, [], dividend)
      | otherwise =
        let (c1, q1, r1) = divisionStep dividend divisor
            (c2, q2, r2) = divide' r1
        in (c1*c2, multiplyTermByTerm (Term c2 0) q1 : q2, r2)

quotient :: Polynomial -> Polynomial -> Polynomial
quotient dividend divisor = q
  where
    (_,q,_) = divide dividend divisor

remainder :: Polynomial -> Polynomial -> Polynomial
remainder dividend divisor = r
  where
    (_,_,r) = divide dividend divisor

content :: Polynomial -> Coeff
content (Polynomial []) = error "attempt to take content of zero polynomial"
content (Polynomial (t:ts)) = content' (abs (termCoeff t)) ts
  where
    content' c [] = c
    content' c (u:us) = content' (gcd c (termCoeff u)) us

signedContent :: Polynomial -> Coeff
signedContent p = signum (leadingCoeff p) * content p

divideByConstant :: Coeff -> Polynomial -> Polynomial
divideByConstant c (Polynomial ts) = Polynomial $ dbc ts
  where
    dbc [] = []
    dbc ((Term coeff expo):rest) = Term (coeff `div` c) expo : dbc rest

divideBySignedContent :: Polynomial -> Polynomial
divideBySignedContent p = divideByConstant (signedContent p) p

polynomialGcd :: Polynomial -> Polynomial -> Polynomial
polynomialGcd p (Polynomial []) = p
polynomialGcd (Polynomial []) q = q
polynomialGcd p q = multiplyConstantByPolynomial g $ pgcd p' q'
  where
    c = content p
    d = content q
    g = gcd c d
    p' = divideBySignedContent p
    q' = divideBySignedContent q
    pgcd u (Polynomial []) = u
    pgcd u v =
      let w = remainder u v
          w' = divideBySignedContent w
      in pgcd v w'
