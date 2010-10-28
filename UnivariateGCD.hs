module UnivariateGCD where

import Data.Ratio (Rational, numerator, denominator, (%))
import Data.List (sort)
import PrimeList
import Test.HUnit

type Term = (Integer,Integer)

-- Polynomial must be passed as a list of (exponent,coefficient)
-- pairs.
-- FIXME: Need to sanitize the input.
univariateGCD :: [(Integer,Rational)] -> [(Integer,Rational)] ->
                 [(Integer,Rational)]
univariateGCD f g = map (\(e,c) -> (e,c%1)) (polynomialGCDInteger (sanitize f) (sanitize g))

polynomialGCDInteger :: [Term] -> [Term] -> [Term]
polynomialGCDInteger = undefined

sanitize :: [(Integer,Rational)] -> [Term]
sanitize = clearContent . removeZeroTerms . reverse . sort . clearDenominators

clearDenominators :: [(Integer,Rational)] -> [Term]
clearDenominators ts = clearDenominators' m ts where
  m = lcmDenominators (map snd ts)

lcmDenominators :: [Rational] -> Integer
lcmDenominators [] = 1
lcmDenominators (x:xs) = lcm (denominator x) (lcmDenominators xs)

clearDenominators' :: Integer -> [(Integer,Rational)] -> [Term]
clearDenominators' _ [] = []
clearDenominators' m ((e,c):ts) = (e,numerator c*(m `div` denominator c)) : clearDenominators' m ts

clearContent :: [Term] -> [Term]
clearContent ts = map (\(e,c) -> (e,div c g)) ts where
  g = gcdList (map snd ts)

-- NOTE: Some things probably screw up when the LC is negative
gcdList :: [Integer] -> Integer
gcdList [] = 1
gcdList [n] = n
gcdList (n:ns) = gcd n (gcdList ns)

reducePolyMod :: Integer -> [Term] -> [Term]
reducePolyMod p = removeZeroTerms . map (reduceTermMod p)

reduceTermMod :: Integer -> Term -> Term
reduceTermMod p (e,c) = (e,mod c p)

removeZeroTerms :: [Term] -> [Term]
removeZeroTerms ((_,0):ts) = removeZeroTerms ts
removeZeroTerms (t:ts) = t : removeZeroTerms ts
removeZeroTerms [] = []

multiplyTerms :: Term -> Term -> Term
multiplyTerms (e1,c1) (e2,c2) = (e1+e2,c1*c2)

multiplyTermByPoly :: Term -> [Term] -> [Term]
multiplyTermByPoly t = map (multiplyTerms t)

multiplyTermByPolyMod :: Integer -> Term -> [Term] -> [Term]
multiplyTermByPolyMod p t ts = reducePolyMod p $ multiplyTermByPoly t ts

degree :: [Term] -> Integer
degree [] = (-1)
degree ((e,_):_) = e

subtractPoly :: [Term] -> [Term] -> [Term]
subtractPoly f [] = f
subtractPoly [] g = multiplyTermByPoly (0,(-1)) g
subtractPoly ((e1,c1):f) ((e2,c2):g)
  | e1 > e2 = (e1,c1) : subtractPoly f ((e2,c2):g)
  | e1 < e2 = (e2,(-c2)) : subtractPoly ((e1,c1):f) g
  | e1 == e2 = (e1,c1-c2) : subtractPoly f g

subtractPolyMod :: Integer -> [Term] -> [Term] -> [Term]
subtractPolyMod p f g = reducePolyMod p (subtractPoly f g)

divideTermMod :: Integer -> Term -> Term -> Term
divideTermMod p (e1,c1) (e2,c2) = (e1-e2,(c1 * inverseMod p c2) `mod` p)

inverseMod :: Integer -> Integer -> Integer
inverseMod p 1 = 1
inverseMod p a = (n * p + 1) `div` a
  where n = a - inverseMod a (p `mod` a)

-- Modulus -> Divisor -> Dividend -> (Quotient,Remainder)
polynomialDivideByMod :: Integer -> [Term] -> [Term] -> ([Term],[Term])
polynomialDivideByMod p d f
  | degree f < degree d = ([],f)
  | otherwise = (thisq:subq,subr)
    where
      (subq,subr) = polynomialDivideByMod p d subf
      subf = subtractPolyMod p f (multiplyTermByPolyMod p thisq d)
      thisq = divideTermMod p (head f) (head d)

polynomialGCDMod :: Integer -> [Term] -> [Term] -> [Term]
polynomialGCDMod _ [] g = g
polynomialGCDMod _ f [] = f
polynomialGCDMod p f g = polynomialGCDMod p g h
  where
    (_,h) = polynomialDivideByMod p g f

setLeadingCoeffMod :: Integer -> Integer -> [Term] -> [Term]
setLeadingCoeffMod p k f =
  multiplyTermByPolyMod p (0,k) (makeMonicMod p f)

makeMonicMod :: Integer -> [Term] -> [Term]
makeMonicMod p f@((e,c):ts) = multiplyTermByPolyMod p (0,inverseMod p c) f

test_UnivariateGCD = [
  reducePolyMod 3 [(18,6),(11,1),(9,(-8)),(2,(-1)),(0,5)] ~?=
  [(11,1),(9,1),(2,2),(0,2)],
  reducePolyMod 5 [(11,5),(8,100),(2,(-5)),(0,10)] ~?= [],
  multiplyTerms (7,3) (5,4) ~?= (12,12),
  multiplyTermByPoly (2,2) [(7,3),(5,8),(4,1),(3,11),(0,2)] ~?=
  [(9,6),(7,16),(6,2),(5,22),(2,4)],
  multiplyTermByPolyMod 13 (2,2) [(7,3),(5,8),(4,1),(3,11),(0,2)] ~?=
  [(9,6),(7,3),(6,2),(5,9),(2,4)]
  ]
