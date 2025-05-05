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
polynomialGCDInteger f g = goUntilDivides firstgood f g restgood
  where
    (firstgood:restgood) = goodList f g

goUntilDivides :: (Integer,[Term]) -> [Term] -> [Term] -> [(Integer,[Term])] -> [Term]
goUntilDivides sofar f g (next:rest)
  | snd (polynomialDivideBy (snd sofar) f) == [] &&
    snd (polynomialDivideBy (snd sofar) g) == [] = (snd sofar)
  | degree (snd next) < degree (snd sofar) = goUntilDivides next f g rest
  | otherwise = goUntilDivides (chinesePolys sofar next) f g rest

chinesePolys :: (Integer,[Term]) -> (Integer,[Term]) -> (Integer,[Term])
chinesePolys (p,f) (q,g) = liftToIntegers (p*q,chinesePolys' (p,f) (q,g))

chinesePolys' :: (Integer,[Term]) -> (Integer,[Term]) -> [Term]
chinesePolys' (p,(e1,c1):fs) (q,(e2,c2):gs)
  | e1 > e2 = (e1,chinese c1 p 0 q) : chinesePolys' (p,fs) (q,(e2,c2):gs)
  | e2 > e1 = (e2,chinese 0 p c2 q) : chinesePolys' (p,(e1,c1):fs) (q,gs)
  | otherwise = (e1,chinese c1 p c2 q) : chinesePolys' (p,fs) (q,gs)
chinesePolys' (p,(e1,c1):fs) (q,[]) = (e1,chinese c1 p 0 q) : chinesePolys' (p,fs) (q,[])
chinesePolys' (p,[]) (q,(e2,c2):gs) = (e2,chinese 0 p c2 q) : chinesePolys' (p,[]) (q,gs)
chinesePolys' (p,[]) (q,[]) = []

-- a p b q -> chinese(a mod p, b mod q) = result mod p*q
chinese :: Integer -> Integer -> Integer -> Integer -> Integer
chinese a p b q = (b * g1 * p + a * g2 * q) `mod` (p*q)
  where
    (_, g1, g2) = fullgcd p q

-- a -> b -> (g, m, n) such that gcd(a,b) = g = m*a + n*b
fullgcd :: Integer -> Integer -> (Integer, Integer, Integer)
fullgcd 0 b = (abs b, 0, signum b)
fullgcd a 0 = (abs a, signum a, 0)
fullgcd a b = (g, n - m * (b `div` a), m)
  where
    (g, m, n) = fullgcd (b `mod` a) a

goodList :: [Term] -> [Term] -> [(Integer,[Term])]
goodList f g = map liftToIntegers $
               filter (\(p,_) -> lcmLC `mod` p /= 0)
               (polynomialGCDListModPrimes f g)
  where
    lcmLC = lcm (snd (head f)) (snd (head g))

liftToIntegers :: (Integer,[Term]) -> (Integer,[Term])
liftToIntegers (p,ts) = (p, liftToIntegers' p ts)

liftToIntegers' :: Integer -> [Term] -> [Term]
liftToIntegers' _ [] = []
liftToIntegers' p ((e,c):ts)
  | c <= p `div` 2 = (e,c):liftToIntegers' p ts
  | otherwise = (e,c-p):liftToIntegers' p ts

polynomialGCDListModPrimes :: [Term] -> [Term] -> [(Integer,[Term])]
polynomialGCDListModPrimes f g = [
  (p, setLeadingCoeffMod p k (polynomialGCDMod p (reducePolyMod p f)
                              (reducePolyMod p g))) |
  p <- primeList]
  where k = gcd (snd (head f)) (snd (head g))

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
  | e1 == e2 && c1 == c2 = subtractPoly f g
  | e1 == e2 = (e1,c1-c2) : subtractPoly f g

subtractPolyMod :: Integer -> [Term] -> [Term] -> [Term]
subtractPolyMod p f g = reducePolyMod p (subtractPoly f g)

divideTermMod :: Integer -> Term -> Term -> Term
divideTermMod p (e1,c1) (e2,c2) = (e1-e2,(c1 * inverseMod p c2) `mod` p)

divideTerm :: Term -> Term -> Term
divideTerm (e1,c1) (e2,c2)
  | c1 `mod` c2 == 0 = (e1-e2,c1 `div` c2)
  | otherwise = error "Problem dividing terms"

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

polynomialDivideBy :: [Term] -> [Term] -> ([Term],[Term])
polynomialDivideBy d f
  | degree f < degree d = ([],f)
  | (snd $ head f) `mod` (snd $ head d) /= 0 = ([],[(1,1)]) -- what a HORRIBLE HACK
  | otherwise = (thisq:subq,subr)
    where
      (subq,subr) = polynomialDivideBy d subf
      subf = subtractPoly f (multiplyTermByPoly thisq d)
      thisq = divideTerm (head f) (head d)

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
