module Expression (eRat, eVar, eSum, eProd, eIntPow, eCall,
                   eMatch, isRational, isNegPow, eAsSum, eTransform,
                   prodAsQuot, extractConstantFromProduct,
                   builtinFunctions, builtinCommands,
                   unsafeExpressionConstructors,
                   Expression,
                   test_Expression) where

import Data.Char (isAlpha, isAlphaNum)
import Data.List
import Data.Ratio ((%), numerator, denominator)
import Control.Exception as C
import Test.HUnit
import HCException

builtinFunctions :: [String]
builtinFunctions = ["expand", "factor", "substitute", "together"]

builtinCommands :: [String]
builtinCommands = ["clear", "help", "exit"]

{-
Expressions only exist in certain forms.
* Variable names begin with an alphabetic character, and consist of
  alphanumeric characters.
* Constants can be any rational number.
* Sums do not (directly) contain sums, nor do products contain products.
* Sums and products are sorted, with constant last (if any).
* Like terms of a sum are combined together (via coefficients).
* Like terms of a product are combined together (via integer powers).
* Sums do not contain a zero.
* Products do not contain a zero or one.
* Sums and products have at least two terms.
* Only variables, sums, and calls are raised to integer powers.
  (Integer powers of constants are evaluated, integer powers of products
  are changed into products of integer powers, and integer powers of
  integer powers are simplified.)
-}

data Expression = ExpressionVariable String |
                  ExpressionProduct [Expression] |
                  ExpressionIntPow Expression Integer |
                  ExpressionCall String [Expression] |
                  ExpressionSum [Expression] |
                  ExpressionRational Rational
  deriving (Eq, Ord, Show)

eMatch :: (Rational -> a) -> (String -> a) -> ([Expression] -> a) ->
          ([Expression] -> a) -> (Expression -> Integer -> a) ->
          (String -> [Expression] -> a) -> Expression -> a
eMatch f _ _ _ _ _ (ExpressionRational n) = f n
eMatch _ f _ _ _ _ (ExpressionVariable s) = f s
eMatch _ _ f _ _ _ (ExpressionSum es) = f es
eMatch _ _ _ f _ _ (ExpressionProduct es) = f es
eMatch _ _ _ _ f _ (ExpressionIntPow e n) = f e n
eMatch _ _ _ _ _ f (ExpressionCall g xs) = f g xs

eTransform :: (Rational -> Expression) -> (String -> Expression) ->
              ([Expression] -> Expression) -> ([Expression] -> Expression) ->
              (Expression -> Integer -> Expression) ->
              (String -> [Expression] -> Expression) -> Expression -> Expression
eTransform p q r s t u =
  eMatch p q (r . map myself) (s . map myself) (\e n -> t (myself e) n)
    (\g xs -> u g (map myself xs))
  where
    myself :: Expression -> Expression
    myself = eTransform p q r s t u

transformSummandForSorting :: Expression -> Expression
transformSummandForSorting = eMatch ExpressionRational wrapVariable
  undefined wrapProduct wrapIntPow wrapCall
  where
    wrapVariable v = ExpressionProduct
      [ExpressionIntPow (ExpressionVariable v) (-1), ExpressionRational 1]
    wrapProduct es = ExpressionProduct $ map transformFactorForSorting $
      es ++ [ExpressionRational 1]
    wrapIntPow e n = ExpressionProduct
      [ExpressionIntPow (transformBaseForSorting e) (-n),
       ExpressionRational 1]
    wrapCall f es = ExpressionProduct
      [ExpressionIntPow (ExpressionCall f
                         (map transformParameterForSorting es)) (-1),
       ExpressionRational 1]

transformFactorForSorting :: Expression -> Expression
transformFactorForSorting = eMatch ExpressionRational wrapVariable
  wrapSum undefined wrapIntPow wrapCall
  where
    wrapVariable v = ExpressionIntPow (ExpressionVariable v) (-1)
    wrapSum es = flip ExpressionIntPow (-1) $ ExpressionSum $
      map transformSummandForSorting $ es ++ [ExpressionRational 0]
    wrapIntPow e n = ExpressionIntPow (transformBaseForSorting e) (-n)
    wrapCall f es = ExpressionIntPow (ExpressionCall f
                                      (map transformParameterForSorting es))
                    (-1)

transformBaseForSorting :: Expression -> Expression
transformBaseForSorting = eMatch undefined ExpressionVariable
  wrapSum undefined undefined wrapCall
  where
    wrapSum es = ExpressionSum $ map transformSummandForSorting es
    wrapCall f es = ExpressionCall f (map transformParameterForSorting es)

transformParameterForSorting :: Expression -> Expression
transformParameterForSorting = eMatch ExpressionRational wrapVariable wrapSum
  wrapProduct wrapIntPow wrapCall
  where
    wrapVariable v = ExpressionSum [
      ExpressionProduct [ExpressionIntPow (ExpressionVariable v) (-1),
                          ExpressionRational 1], ExpressionRational 0]
    wrapSum es = ExpressionSum $ map transformSummandForSorting $
      es ++ [ExpressionRational 0]
    wrapProduct es = ExpressionSum [
      ExpressionProduct (map transformFactorForSorting
                         (es ++ [ExpressionRational 1])), ExpressionRational 0]
    wrapIntPow e n = ExpressionSum [
      ExpressionProduct [ExpressionIntPow (transformBaseForSorting e) (-n),
                         ExpressionRational 1], ExpressionRational 0]
    wrapCall f es = ExpressionSum [
      ExpressionProduct [
          ExpressionIntPow
            (ExpressionCall f (map transformParameterForSorting es)) (-1),
          ExpressionRational 1], ExpressionRational 0]

prodAsQuot :: [Expression] -> (Integer,[Expression],[Expression])
prodAsQuot [] = (1,[],[])
prodAsQuot [ExpressionRational q] =
  case (absnum,den) of
    (1,1) -> (sign,[               ],[            ])
    (_,1) -> (sign,[eRat (absnum%1)],[            ])
    (1,_) -> (sign,[               ],[eRat (den%1)])
    (_,_) -> (sign,[eRat (absnum%1)],[eRat (den%1)])
    where
      sign = signum num
      absnum = abs num
      num = numerator q
      den = denominator q
prodAsQuot (e@(ExpressionIntPow b n):es)
  | n < 0 = (s,ns,eIntPow b (-n):ds)
  | otherwise = (s,e:ns,ds)
    where
      (s,ns,ds) = prodAsQuot es
prodAsQuot (e:es) = (s,e:ns,ds)
  where
    (s,ns,ds) = prodAsQuot es

extractConstantFromProduct :: [Expression] -> ([Expression],Rational)
extractConstantFromProduct xs = (es,r')
  where
    (r,es) = partition isRational xs
    r' = case r of
      [] -> 1
      [ExpressionRational x] -> x
      _ -> undefined

true :: a -> Bool
true = const True
false :: a -> Bool
false = const False

isRational :: Expression -> Bool
isRational = eMatch true false false false (const false) (const false)

isNegPow :: Expression -> Bool
isNegPow = eMatch false false false false (\_ n -> n<0) (const false)

-- Note: might want 0 -> [] instead of 0 -> [0]
eAsSum :: Expression -> [Expression]
eAsSum =
  eMatch (list . eRat) (list . eVar) id (list . eProd) (\e n -> [eIntPow e n])
    (\f es -> [eCall f es])
  where
    list :: a -> [a]
    list x = [x]

unsafeExpressionConstructors ::
  (Rational -> Expression, String -> Expression,
   [Expression] -> Expression, [Expression] -> Expression,
   Expression -> Integer -> Expression, String -> [Expression] -> Expression)
unsafeExpressionConstructors =
  (ExpressionRational, ExpressionVariable,
   ExpressionSum, ExpressionProduct,
   ExpressionIntPow, ExpressionCall)

-------------------- RATIONALS --------------------

-- This serves little purpose now.
eRat :: Rational -> Expression
eRat n = ExpressionRational n

-------------------- VARIABLES --------------------

eVar :: String -> Expression
eVar "" = error "invalid variable name"
eVar vv@(v:vs)
  | elem vv (builtinFunctions ++ builtinCommands) = throw HCReservedWord
  | isAlpha v && all isAlphaNum vs = ExpressionVariable vv
  | otherwise = error "invalid variable name"

-------------------- SUMS --------------------

-- Return a sum in standard form, assuming that all summands are
-- already in standard form.
-- Invariants, in order of processing:
--   1. a sum contains no sums
--   2. a sum is sorted
--   3. like terms of a sum are combined together, and removed if zero
--   4. a sum contains at least 2 elements
eSum :: [Expression] -> Expression
eSum exprs = makeSum $
             combineSummands $
             sortOn transformSummandForSorting $
             flattenSummands $
             exprs

flattenSummands :: [Expression] -> [Expression]
flattenSummands (ExpressionSum summands:es) = summands ++ flattenSummands es
flattenSummands (e:es) = e:flattenSummands es
flattenSummands [] = []

combineSummands :: [Expression] -> [Expression]
combineSummands es = map pushCoeff $ combineTerms $ map popCoeff es
popCoeff :: Expression -> (Rational,Expression)
popCoeff (ExpressionRational n) = (n,eRat 1)
popCoeff (ExpressionProduct ps) = getCoefficient 1 ps []
  where
    getCoefficient :: Rational -> [Expression] -> [Expression] ->
      (Rational,Expression)
    getCoefficient d [] result = (d,eProd (reverse result))
    getCoefficient _ [ExpressionRational n] result = (n,eProd (reverse result))
    getCoefficient d (e:es) result = getCoefficient d es (e:result)
popCoeff x = (1,x)
pushCoeff :: (Rational,Expression) -> Expression
pushCoeff (c,e) = eProd [eRat c,e]

makeSum :: [Expression] -> Expression
makeSum [] = eRat 0
makeSum [e] = e
makeSum es = ExpressionSum es

-------------------- PRODUCTS --------------------

-- Return a product in standard form, assuming that all factors are
-- already in standard form.
-- Invariants, in order of processing:
--   1. a product contains no products
--   2. a product is sorted
--   3. a product contains at most one constant, which is removed if 1
--   4. a product of zero with anything is zero
--   5. a product contains at least 2 elements
eProd :: [Expression] -> Expression
eProd exprs = makeProduct $
              detectZeroFactors $
              combineConstantFactors $
              combineFactors $
              sortOn transformFactorForSorting $
              flattenFactors $
              exprs

flattenFactors :: [Expression] -> [Expression]
flattenFactors (ExpressionProduct terms:es) = terms ++ flattenFactors es
flattenFactors (e:es) = e:flattenFactors es
flattenFactors [] = []

combineFactors :: [Expression] -> [Expression]
combineFactors es = map pushPower $ combineTerms $ map popPower es
popPower :: Expression -> (Integer,Expression)
popPower (ExpressionIntPow e n) = (n,e)
popPower e = (1,e)
pushPower :: (Integer,Expression) -> Expression
pushPower = uncurry (flip eIntPow)

combineConstantFactors :: [Expression] -> [Expression]
combineConstantFactors (ExpressionRational m:ExpressionRational n:es) =
  combineConstantFactors (eRat (m*n):es)
combineConstantFactors (ExpressionRational 1:es) = es
combineConstantFactors (e:es) = e : combineConstantFactors es
combineConstantFactors [] = []

detectZeroFactors :: [Expression] -> [Expression]
detectZeroFactors es
  | any (== ExpressionRational 0) es = [eRat 0]
  | otherwise = es

makeProduct :: [Expression] -> Expression
makeProduct [] = eRat 1
makeProduct [e] = e
makeProduct es = ExpressionProduct es

-------------------- COMMON CODE FOR SUMS & PRODUCTS --------------------

combineTerms :: (Eq a, Num a) => [(a,Expression)] -> [(a,Expression)]
combineTerms ((0,_):es) = combineTerms es
combineTerms ((m,e):(n,f):gs)
  | e == f    = combineTerms ((m+n,e):gs)
  | otherwise = (m,e):combineTerms ((n,f):gs)
combineTerms es = es

-------------------- INTEGER POWERS --------------------

eIntPow :: Expression -> Integer -> Expression
eIntPow e 1 = e
eIntPow _ 0 = eRat 1
eIntPow (ExpressionProduct xs) n = eProd $ map (flip eIntPow n) xs
eIntPow (ExpressionIntPow x m) n = eIntPow x (m*n)
eIntPow (ExpressionRational x) n
  | n < 0 && x == 0 = C.throw HCDivideByZero
  | n < 0           = eRat (recip (x^(-n)))
  | otherwise       = eRat (x^n)
eIntPow x n = ExpressionIntPow x n

-------------------- CALLS --------------------

eCall :: String -> [Expression] -> Expression
eCall "" _ = error "invalid function name"
eCall gg@(g:gs) xs
  | gg `elem` builtinCommands = throw HCReservedWord
  | isAlpha g && all isAlphaNum gs = ExpressionCall gg xs
  | otherwise = error "invalid function name"

-------------------- TESTS --------------------

test_Expression :: [Test]
test_Expression =
  [ eSum [x,y,eProd [eRat (-1),y]] ~?= x
  , eProd [x,y,eIntPow y (-1)] ~?= x
  , ([x,y],3) ~?= extractConstantFromProduct [x,y,eRat 3]
  , ([x,y],1) ~?= extractConstantFromProduct [x,y]
  ]
  where
    x = eVar "x"
    y = eVar "y"
