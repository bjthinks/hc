module Factor.Tests where

import Test.HUnit
import Factor.Defs
import Factor.Polynomial
import Factor.ParsePolynomial
import Factor.ModularPolynomial
import Factor.SquareFree

printPolynomialTests :: Test
printPolynomialTests = TestList [
  "PrettyPrint 0" ~: "0" ~=? printPolynomial (Polynomial []),
  "PrettyPrint 1" ~: "1" ~=? printPolynomial (Polynomial [Term 1 0]),
  "PrettyPrint 5" ~: "5" ~=? printPolynomial (Polynomial [Term 5 0]),
  "PrettyPrint -1" ~: "-1" ~=? printPolynomial (Polynomial [Term (-1) 0]),
  "PrettyPrint -7" ~: "-7" ~=? printPolynomial (Polynomial [Term (-7) 0]),
  "PrettyPrint x" ~: "x" ~=? printPolynomial (Polynomial [Term 1 1]),
  "PrettyPrint 3x" ~: "3x" ~=? printPolynomial (Polynomial [Term 3 1]),
  "PrettyPrint -x" ~: "-x" ~=? printPolynomial (Polynomial [Term (-1) 1]),
  "PrettyPrint -5x" ~: "-5x" ~=? printPolynomial (Polynomial [Term (-5) 1]),
  "PrettyPrint x^2" ~: "x^2" ~=? printPolynomial (Polynomial [Term 1 2]),
  "PrettyPrint 3x^2" ~: "3x^2" ~=? printPolynomial (Polynomial [Term 3 2]),
  "PrettyPrint -x^2" ~: "-x^2" ~=? printPolynomial (Polynomial [Term (-1) 2]),
  "PrettyPrint -5x^2" ~: "-5x^2" ~=? printPolynomial (Polynomial [Term (-5) 2]),
  "PrettyPrint -53x^27" ~: "-53x^27" ~=?
    printPolynomial (Polynomial [Term (-53) 27]),
  "PrettyPrint x+1" ~: "x + 1" ~=?
    printPolynomial (Polynomial [Term 1 1, Term 1 0]),
  "PrettyPrint x-1" ~: "x - 1" ~=?
    printPolynomial (Polynomial [Term 1 1, Term (-1) 0]),
  "PrettyPrint -x+1" ~: "-x + 1" ~=?
    printPolynomial (Polynomial [Term (-1) 1, Term 1 0]),
  "PrettyPrint -x-1" ~: "-x - 1" ~=?
    printPolynomial (Polynomial [Term (-1) 1, Term (-1) 0]),
  "PrettyPrint 5x+3" ~: "5x + 3" ~=?
    printPolynomial (Polynomial [Term 5 1, Term 3 0]),
  "PrettyPrint 5x-3" ~: "5x - 3" ~=?
    printPolynomial (Polynomial [Term 5 1, Term (-3) 0]),
  "PrettyPrint -5x+3" ~: "-5x + 3" ~=?
    printPolynomial (Polynomial [Term (-5) 1, Term 3 0]),
  "PrettyPrint -5x-3" ~: "-5x - 3" ~=?
    printPolynomial (Polynomial [Term (-5) 1, Term (-3) 0]),
  "PrettyPrint x^2+x+1" ~: "x^2 + x + 1" ~=?
    printPolynomial (Polynomial [Term 1 2, Term 1 1, Term 1 0]),
  "PrettyPrint -x^2+x+1" ~: "-x^2 + x + 1" ~=?
    printPolynomial (Polynomial [Term (-1) 2, Term 1 1, Term 1 0]),
  "PrettyPrint x^2-x+1" ~: "x^2 - x + 1" ~=?
    printPolynomial (Polynomial [Term 1 2, Term (-1) 1, Term 1 0]),
  "PrettyPrint 4x^2+4x+1" ~: "4x^2 + 4x + 1" ~=?
    printPolynomial (Polynomial [Term 4 2, Term 4 1, Term 1 0]),
  "PrettyPrint -4x^2-4x-1" ~: "-4x^2 - 4x - 1" ~=?
    printPolynomial (Polynomial [Term (-4) 2, Term (-4) 1, Term (-1) 0]),
  () ~=? ()]

makePolynomialTests :: Test
makePolynomialTests = TestList [
  "0" ~=? printPolynomial (makePolynomial []),
  "0" ~=? printPolynomial (makePolynomial [Term 0 0]),
  "0" ~=? printPolynomial (makePolynomial [Term 0 1]),
  "0" ~=? printPolynomial (makePolynomial [Term 0 2]),
  "5" ~=? printPolynomial (makePolynomial [Term 3 2, Term 5 0, Term (-3) 2]),
  "1" ~=? printPolynomial (makePolynomial [Term 1 0]),
  "x + 1" ~=? printPolynomial
    (makePolynomial [Term 1 0, Term 2 1, Term (-1) 1]),
  "x - 1" ~=? printPolynomial
    (makePolynomial [Term 1 0, Term 2 1, Term (-2) 0, Term (-1) 1]),
  "-x + 1" ~=? printPolynomial
    (makePolynomial [Term (-2) 1, Term 1 0, Term 2 1, Term (-1) 1]),
  "-x - 1" ~=? printPolynomial
    (makePolynomial [Term (-2) 1, Term 1 0, Term 2 1, Term (-1) 1,
                     Term (-2) 0]),
  () ~=? ()]

parsePolynomialTests :: Test
parsePolynomialTests = TestList [
  "0" ~=? printPolynomial (parsePolynomial "0"),
  "1" ~=? printPolynomial (parsePolynomial "1"),
  "-1" ~=? printPolynomial (parsePolynomial "-1"),
  "5" ~=? printPolynomial (parsePolynomial "5"),
  "x" ~=? printPolynomial (parsePolynomial "x"),
  "-x" ~=? printPolynomial (parsePolynomial "-x"),
  "3x" ~=? printPolynomial (parsePolynomial "3x"),
  "-7x" ~=? printPolynomial (parsePolynomial "-7x"),
  "x + 1" ~=? printPolynomial (parsePolynomial "1+x"),
  "x - 1" ~=? printPolynomial (parsePolynomial "-1+x"),
  "-x + 1" ~=? printPolynomial (parsePolynomial "1-x"),
  "-x - 1" ~=? printPolynomial (parsePolynomial "-1-x"),
  "x^2" ~=? printPolynomial (parsePolynomial "x^2"),
  "x^2" ~=? printPolynomial (parsePolynomial "1x^2"),
  "-x^5" ~=? printPolynomial (parsePolynomial "-1x^5"),
  "-x^2 + 4x - 4" ~=? printPolynomial (parsePolynomial "3x-2-x^2-2+x"),
  "1" ~=? printPolynomial (parsePolynomial "+1"),
  () ~=? ()]

p :: Polynomial
p = parsePolynomial "  3   x ^  2 + 5 x   - 7  "

q :: Polynomial
q = parsePolynomial "-8x+2"

r :: Polynomial
r = parsePolynomial "-x^3+2x^2+8x+3"

polynomialIsNum :: Test
polynomialIsNum = TestList [
  "57" ~=? printPolynomial (fromInteger 57),
  "2" ~=? printPolynomial (parsePolynomial "1" + parsePolynomial "1"),
  "3x^2 - 3x - 5" ~=? printPolynomial (p+q),
  "-x^3 + 5x^2 + 13x - 4" ~=? printPolynomial (p+r),
  "-x^3 + 2x^2 + 5" ~=? printPolynomial (q+r),
  "-3x^2 - 5x + 7" ~=? printPolynomial (-p),
  "8x - 2" ~=? printPolynomial (-q),
  "x^3 - 2x^2 - 8x - 3" ~=? printPolynomial (-r),
  "-24x^3 - 34x^2 + 66x - 14" ~=? printPolynomial (p*q),
  "-3x^5 + x^4 + 41x^3 + 35x^2 - 41x - 21" ~=? printPolynomial (p*r),
  "8x^4 - 18x^3 - 60x^2 - 8x + 6" ~=? printPolynomial (q*r),
  "x^2 - 1" ~=? printPolynomial (parsePolynomial "x+1" * parsePolynomial "x-1"),
  "x^4 - 4x^3 + 6x^2 - 4x + 1" ~=?
    printPolynomial (parsePolynomial "x-1" ^ (4 :: Int)),
  "36x^4 - 72x^2 + 36" ~=? printPolynomial ((p10*p10)*(p11*p11)),
  "x^4 - 2x^2 + 1" ~=? printPolynomial ((p1*p1)*(p2*p2)),
  () ~=? ()]

polynomialTests :: Test
polynomialTests = TestList
  [printPolynomialTests, makePolynomialTests, parsePolynomialTests,
   polynomialIsNum]

printModularPolynomialTests :: Test
printModularPolynomialTests = TestList [
  "0 mod 5" ~=? printModularPolynomial (ModularPolynomial 5 []),
  "3 mod 5" ~=? printModularPolynomial (ModularPolynomial 5 [Term 3 0]),
  "x^2 mod 5" ~=? printModularPolynomial (ModularPolynomial 5 [Term 1 2]),
  () ~=? ()]

makeModularPolynomialTests :: Test
makeModularPolynomialTests = TestList [
  "4x^2 + 3 mod 5" ~=? printModularPolynomial
    (makeModularPolynomial 5 [Term (-2) 0, Term 3 1, Term 7 1, Term (-1) 2]),
  () ~=? ()]

mp :: ModularPolynomial
mp = parseModularPolynomial " 3 x ^ 2 + 5 x - 7 mod 5 "

mq :: ModularPolynomial
mq = parseModularPolynomial "-8x+2 mod 5"

mr :: ModularPolynomial
mr = parseModularPolynomial "-x^3+2x^2+8x+3 mod 5"

parseModularPolynomialTests :: Test
parseModularPolynomialTests = TestList [
  "3x^2 + 3 mod 5" ~=? printModularPolynomial mp,
  "2x + 2 mod 5" ~=? printModularPolynomial mq,
  "4x^3 + 2x^2 + 3x + 3 mod 5" ~=? printModularPolynomial mr,
  () ~=? ()]

modularPolynomialIsNum :: Test
modularPolynomialIsNum = TestList [
  "3x^2 + 2x mod 5" ~=? printModularPolynomial (mp + mq),
  "4x^3 + 3x + 1 mod 5" ~=? printModularPolynomial (mp + mr),
  "4x^3 + 2x^2 mod 5" ~=? printModularPolynomial (mq + mr),
  "x^3 + x^2 + x + 1 mod 5" ~=? printModularPolynomial (mp * mq),
  "2x^5 + x^4 + x^3 + 4x + 4 mod 5" ~=? printModularPolynomial (mp * mr),
  "3x^4 + 2x^3 + 2x + 1 mod 5" ~=? printModularPolynomial (mq * mr),
  "2x^2 + 2 mod 5" ~=? printModularPolynomial (negate mp),
  "3x + 3 mod 5" ~=? printModularPolynomial (negate mq),
  "x^3 + 3x^2 + 2x + 2 mod 5" ~=? printModularPolynomial (negate mr),
  () ~=? ()]

modularPolynomialTests :: Test
modularPolynomialTests = TestList
  [printModularPolynomialTests, makeModularPolynomialTests,
    parseModularPolynomialTests, modularPolynomialIsNum]

p1 :: Polynomial
p1 = parsePolynomial "x+1"

p2 :: Polynomial
p2 = parsePolynomial "x-1"

p3 :: Polynomial
p3 = parsePolynomial "x^2-2"

p4 :: Polynomial
p4 = parsePolynomial "x^3+3"

p5 :: Polynomial
p5 = parsePolynomial "x+5"

p6 :: Polynomial
p6 = parsePolynomial "10x^3"

p7 :: Polynomial
p7 = parsePolynomial "16"

p8 :: Polynomial
p8 = parsePolynomial "x^3 - x^2 - x + 1"

p9 :: Polynomial
p9 = parsePolynomial "3x^2 - 2x - 1"

p10 :: Polynomial
p10 = parsePolynomial "2x-2"

p11 :: Polynomial
p11 = parsePolynomial "3x+3"

polynomialOpTests :: Test
polynomialOpTests = TestList [
  "6x + 5" ~=? printPolynomial (derivative p),
  "-8" ~=? printPolynomial (derivative q),
  "-3x^2 + 4x + 8" ~=? printPolynomial (derivative r),
  "1" ~=? printPolynomial (derivative p1),
  "1" ~=? printPolynomial (derivative p2),
  "2x" ~=? printPolynomial (derivative p3),
  "3x^2" ~=? printPolynomial (derivative p4),
  "1" ~=? printPolynomial (derivative p5),
  Term 3 2 ~=? leadingTerm p,
  Term (-8) 1 ~=? leadingTerm q,
  Term (-1) 3 ~=? leadingTerm r,
  Term 1 1 ~=? leadingTerm p1,
  Term 1 1 ~=? leadingTerm p2,
  Term 1 2 ~=? leadingTerm p3,
  Term 1 3 ~=? leadingTerm p4,
  Term 1 1 ~=? leadingTerm p5,
  (-8,"3x","-46x + 56") ~=? showStep (divisionStep p q),
  (3,"-x","11x^2 + 17x + 9") ~=? showStep (divisionStep r p),
  (-8,"-x^2","-14x^2 - 64x - 24") ~=? showStep (divisionStep r q),
  (-4,"5x^2","-10x^2") ~=? showStep (divisionStep p6 q),
  (10,"-1","20x^2 + 80x + 30") ~=? showStep (divisionStep r p6),
  (8,"5x^3","0") ~=? showStep (divisionStep p6 p7),
  (1,"0","0") ~=? showDivision (divide (Polynomial []) p3),
  (1,"0",printPolynomial p5) ~=? showDivision (divide p5 p4),
  (1,"1","2") ~=? showDivision (divide p1 p2),
  (1,"x - 1","-1") ~=? showDivision (divide p3 p1),
  (16,"x + 1","0") ~=? showDivision (divide p1 p7),
  (16,"x^3 + 3","0") ~=? showDivision (divide p4 p7),
  (9,"3x - 1","-8x + 8") ~=? showDivision (divide p8 p9),
  (4,"x + 2","0") ~=?
    showDivision (divide (parsePolynomial "2x+4") (parsePolynomial "8")),
  (8,"4x^4 + 2x^2 - 127","8x - 159") ~=?
    showDivision (divide (parsePolynomial "x^6-32x^2+x-4")
                  (parsePolynomial "2x^2-1")),
  1 ~=? content p1,
  1 ~=? content p4,
  10 ~=? content p6,
  16 ~=? content p7,
  2 ~=? content (parsePolynomial "8x^3-6x^2+16x-30"),
  p2 ~=? divideByConstant 2 p10,
  p1 ~=? divideByConstant 3 p11,
  p1 ~=? polynomialGcd p1 (p1*p2),
  p1 ~=? polynomialGcd (p1*p2) p1,
  p1*p2 ~=? polynomialGcd (p1^(3 :: Int) * p2) (p1 * p2^(5 :: Int)),
  p10^(2::Int)*p11^(2::Int) ~=?
    polynomialGcd (p10^(5::Int) * p11^(2::Int)) (p10^(2::Int) * p11^(3::Int)),
  p1 ~=? polynomialGcd p1 (Polynomial []),
  p1 ~=? polynomialGcd (Polynomial []) p1,
  constantPolynomial 2 * p1*p2*p3*p4 ~=?
    polynomialGcd (p1*p2*p3*p4*p5*p6) (p1*p2*p3*p4*p7*p8),
  "1" ~=? printPolynomial (polynomialGcd (parsePolynomial "x^2-1")
                           (parsePolynomial "2x")),
  () ~=? ()]
  where
    showStep (constant, quoti, remain)
      = (constant, printPolynomial (Polynomial [quoti]),
         printPolynomial remain)
    showDivision (constant, quoti, remain)
      = (constant, printPolynomial quoti, printPolynomial remain)

m1 :: ModularPolynomial
m1 = parseModularPolynomial "x+1 mod 2"

m2 :: ModularPolynomial
m2 = parseModularPolynomial "x^2+1 mod 2"

m3 :: ModularPolynomial
m3 = parseModularPolynomial "x^7+x^4+x^2+1 mod 2"

m4 :: ModularPolynomial
m4 = parseModularPolynomial "x^3+2 mod 3"

m5 :: ModularPolynomial
m5 = parseModularPolynomial "x+2 mod 3"

m6 :: ModularPolynomial
m6 = parseModularPolynomial "2x^3 mod 3"

m7 :: ModularPolynomial
m7 = parseModularPolynomial "2 mod 3"

m8 :: ModularPolynomial
m8 = parseModularPolynomial "x^3-x^2-x+1 mod 3"

m9 :: ModularPolynomial
m9 = parseModularPolynomial "2x+2 mod 3"

modularPolynomialOpTests :: Test
modularPolynomialOpTests = TestList [
  "1 mod 2" ~=? printModularPolynomial (mDerivative m1),
  "0 mod 2" ~=? printModularPolynomial (mDerivative m2),
  "x^6 mod 2" ~=? printModularPolynomial (mDerivative m3),
  "0 mod 3" ~=? printModularPolynomial (mDerivative m4),
  "1 mod 3" ~=? printModularPolynomial (mDerivative m5),
  "0 mod 3" ~=? printModularPolynomial (mDerivative m6),
  "0 mod 3" ~=? printModularPolynomial (mDerivative m7),
  "x + 2 mod 3" ~=? printModularPolynomial (mDerivative m8),
  "2 mod 3" ~=? printModularPolynomial (mDerivative m9),
  Term 1 1 ~=? mLeadingTerm m1,
  Term 1 2 ~=? mLeadingTerm m2,
  Term 1 7 ~=? mLeadingTerm m3,
  Term 1 3 ~=? mLeadingTerm m4,
  Term 1 1 ~=? mLeadingTerm m5,
  Term 2 3 ~=? mLeadingTerm m6,
  Term 2 0 ~=? mLeadingTerm m7,
  Term 1 3 ~=? mLeadingTerm m8,
  Term 2 1 ~=? mLeadingTerm m9,
  1 ~=? mLeadingCoeff m3,
  2 ~=? mLeadingCoeff m6,
  7 ~=? mDegree m3,
  3 ~=? mDegree m6
  , ("x mod 2","x + 1 mod 2") ~=? showStep (mDivisionStep m2 m1)
  , ("x^5 mod 2","x^5 + x^4 + x^2 + 1 mod 2") ~=? showStep (mDivisionStep m3 m2)
  , ("2x^2 mod 3","2x^2 mod 3") ~=? showStep (mDivisionStep m6 m5)
  , ("2x^2 mod 3","x^2 + 2x + 1 mod 3") ~=? showStep (mDivisionStep m8 m9)
  , ("x + 1 mod 2","0 mod 2") ~=? showDivision (mDivide m2 m1)
  , ("x^5 + x^3 + x^2 + x mod 2","x + 1 mod 2") ~=?
    showDivision (mDivide m3 m2)
  , ("2x^2 + 2x + 2 mod 3","2 mod 3") ~=? showDivision (mDivide m6 m5)
  , ("2x^2 + 2x + 2 mod 3","0 mod 3") ~=? showDivision (mDivide m8 m9)
  , "x + 1 mod 2" ~=? printModularPolynomial (mPolynomialGcd m1 m2)
  , "x + 1 mod 2" ~=? printModularPolynomial (mPolynomialGcd m2 m1)
  , "x + 1 mod 2" ~=? printModularPolynomial (mPolynomialGcd m3 m2)
  , "x + 1 mod 2" ~=? printModularPolynomial (mPolynomialGcd m2 m3)
  , "1 mod 3" ~=? printModularPolynomial (mPolynomialGcd m6 m5)
  , "1 mod 3" ~=? printModularPolynomial (mPolynomialGcd m5 m6)
  , printModularPolynomial (n1*n2*n3*n4) ~=?
    printModularPolynomial (mPolynomialGcd
                            (n1*n2*n3*n4*n5*n7*n7)
                            (n1*n2*n2*n3*n4*n4*n6*n8*n9*n9))
  , "x^6 + 2x^3 + 2 mod 5" ~=? printModularPolynomial (n1*n2*n3*n4)
  , printModularPolynomial n9 ~=?
    printModularPolynomial (mPolynomialGcd
                            (n1*n2*n4*n5*n7*n7*n9*n9*n9)
                            (n3*n3*n6*n6*n8*n8*n8*n9))
  , Nothing ~=? mthRoot n1
  , Nothing ~=? mthRoot n2
  , Nothing ~=? mthRoot n3
  , Nothing ~=? mthRoot n5
  , Nothing ~=? mthRoot n7
  , Just "x^5 + 4x + 2 mod 5" ~=?
    showMaybe (mthRoot (parseModularPolynomial "x^25+4x^5+2 mod 5"))
  , Just m1 ~=? mthRoot m2
  , True ~=? mIsSquareFree (n1*n2*n3*n4)
  , False ~=? mIsSquareFree (n1*n2*n3*n4*n9*n9)
  , True ~=? mIsSquareFree m7
  , True ~=? mIsSquareFree (parseModularPolynomial "0 mod 5")
  , False ~=? mIsSquareFree (n1*n1)
  , False ~=? mIsSquareFree (n1*n1*n1)
  , False ~=? mIsSquareFree (n1*n1*n1*n1)
  ]
  where
    showStep (quoti, remain)
      = (printModularPolynomial (ModularPolynomial (modulus remain) [quoti]),
         printModularPolynomial remain)
    showDivision (quoti, remain)
      = (printModularPolynomial quoti, printModularPolynomial remain)
    showMaybe = fmap printModularPolynomial

-- Here are some irreducibles mod 5:
n1 :: ModularPolynomial
n1 = parseModularPolynomial "x+2 mod 5"
n2 :: ModularPolynomial
n2 = parseModularPolynomial "x+4 mod 5"
n3 :: ModularPolynomial
n3 = parseModularPolynomial "x^2+x+1 mod 5"
n4 :: ModularPolynomial
n4 = parseModularPolynomial "x^2+3x+4 mod 5"
n5 :: ModularPolynomial
n5 = parseModularPolynomial "x^3+2x+1 mod 5"
n6 :: ModularPolynomial
n6 = parseModularPolynomial "x^3+x^2+4x+1 mod 5"
n7 :: ModularPolynomial
n7 = parseModularPolynomial "x^4+2 mod 5"
n8 :: ModularPolynomial
n8 = parseModularPolynomial "x^4+x^3+3x+2 mod 5"
n9 :: ModularPolynomial
n9 = parseModularPolynomial "x^4+2x^3+2x^2+2x+4 mod 5"

squareFreeTests :: Test
squareFreeTests = TestList [
  (1,[("x + 1",1)]) ~=? printResults (squareFree $ p1),
  (1,[("x + 1",2)]) ~=? printResults (squareFree $ p1*p1),
  (1,[("x^2 - 1",1)]) ~=? printResults (squareFree $ p1*p2),
  (1,[("x + 1",2),("x - 1",1)]) ~=? printResults (squareFree $ p1*p1*p2),
  (10,[("x",3)]) ~=? printResults (squareFree p6),
  (1,[("x - 1",2),("x + 1",1)]) ~=? printResults (squareFree p8),
  (12,[("x - 1",2),("x + 1",1)]) ~=? printResults (squareFree $ p10*p10*p11),
  (16*27,[("x - 1",4),("x + 1",3)]) ~=? printResults
    (squareFree $ p10*p10*p10*p10*p11*p11*p11),
  (36,[("x^2 - 1",3),("x^3 + 3",2),("x^2 - 2",1)]) ~=? printResults
    (squareFree $ p1*p2*p3*p4*p4*p10*p10*p11*p11)
  , 2 ~=? goodModulus p1
  , 2 ~=? goodModulus p4
  , 3 ~=? goodModulus (parsePolynomial "x^2+1")
  , 5 ~=? goodModulus (parsePolynomial "x^6+7")
  , 3 ~=? goodModulus (parsePolynomial "x^3+x+2") -- mod 2 is x(x+1)^2
  , False ~=? isGoodModulus 3 p4
  ]
  where
    printResults (Factorization c fs) =
      (c,map (\(poly,expo) -> (printPolynomial poly,expo)) fs)

invertModTests :: Test
invertModTests = TestList
  [1 ~=? (x * (invertMod 101 x)) `mod` 101 | x <- [1..100]]

tests :: Test
tests = TestList [polynomialTests, modularPolynomialTests, polynomialOpTests,
                  modularPolynomialOpTests, squareFreeTests, invertModTests]

runTests :: IO Counts
runTests = runTestTT tests
